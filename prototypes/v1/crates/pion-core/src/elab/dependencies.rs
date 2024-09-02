use pion_hir::syntax::{self as hir, Ident};
use pion_utils::identity::{Identity, PtrMap, PtrSet};
use pion_utils::symbol::{Symbol, SymbolMap};

type ItemSet<'hir> = PtrSet<&'hir hir::Item<'hir>>;
type DependencyGraph<'hir> = PtrMap<&'hir hir::Item<'hir>, ItemSet<'hir>>;

type ItemEnv<'hir> = SymbolMap<&'hir hir::Item<'hir>>;
type LocalEnv = Vec<Symbol>;

pub fn module_sccs<'hir>(module: &hir::Module<'hir>) -> Vec<Vec<&'hir hir::Item<'hir>>> {
    let graph = module_dependency_graph(module);
    find_sccs(&graph)
}

fn module_dependency_graph<'hir>(module: &hir::Module<'hir>) -> DependencyGraph<'hir> {
    let item_env: ItemEnv = module
        .items
        .iter()
        .filter_map(|item| Some((item.name()?.symbol, item)))
        .collect();
    let mut graph = DependencyGraph::default();

    for item in module.items {
        let mut local_env = LocalEnv::default();
        let mut required_items = ItemSet::default();

        match item {
            hir::Item::Def(def) => {
                def_dependencies(def, &mut local_env, &item_env, &mut required_items);
                graph.insert(Identity(item), required_items);
            }
        }
    }

    graph
}

fn find_sccs<'hir>(graph: &DependencyGraph<'hir>) -> Vec<Vec<&'hir hir::Item<'hir>>> {
    struct SccSolver<'graph, 'hir> {
        graph: &'graph DependencyGraph<'hir>,
        ids: PtrMap<&'hir hir::Item<'hir>, i32>,
        stack: Vec<&'hir hir::Item<'hir>>,
        on_stack: PtrMap<&'hir hir::Item<'hir>, bool>,
        id: i32,
        low_link: PtrMap<&'hir hir::Item<'hir>, i32>,
        sccs: Vec<Vec<&'hir hir::Item<'hir>>>,
    }

    const UNVISITED: i32 = -1;

    impl<'graph, 'hir> SccSolver<'graph, 'hir> {
        fn new(graph: &'graph DependencyGraph<'hir>) -> Self {
            Self {
                graph,
                ids: graph.keys().map(|item| (*item, UNVISITED)).collect(),
                sccs: Vec::new(),
                low_link: graph.keys().map(|item| (*item, 0)).collect(),
                id: 0,
                stack: Vec::new(),
                on_stack: graph.keys().map(|item| (*item, false)).collect(),
            }
        }
    }

    fn dfs<'hir>(solver: &mut SccSolver<'_, 'hir>, at: &'hir hir::Item<'hir>) {
        let at = Identity(at);

        solver.low_link.insert(at, solver.id);
        solver.ids.insert(at, solver.id);
        solver.id += 1;
        solver.stack.push(at.0);
        solver.on_stack.insert(at, true);
        // visit all neighbours and min low-link on callback
        for &neighbour in &solver.graph[&at] {
            if solver.ids[&neighbour] == UNVISITED {
                dfs(solver, neighbour.0);
            }
            if solver.on_stack[&neighbour] {
                solver.low_link.insert(
                    at,
                    std::cmp::min(solver.low_link[&at], solver.low_link[&neighbour]),
                );
            }
        }
        // after having visited all the neighbours of `at` if we're at the start of
        // a SCC empty the seen stack until we're back to the start of the SCC
        if solver.ids[&at] == solver.low_link[&at] {
            let mut this_scc = Vec::new();
            while let Some(node) = solver.stack.pop() {
                let node = Identity(node);

                solver.on_stack.insert(node, false);
                solver.low_link.insert(node, solver.ids[&at]);
                this_scc.push(node.0);
                if node == at {
                    solver.sccs.push(this_scc);
                    break;
                }
            }
        }
    }

    let mut solver = SccSolver::new(graph);

    for item in graph.keys() {
        if solver.ids[item] == UNVISITED {
            dfs(&mut solver, item.0);
        }
    }

    solver.sccs
}

fn def_dependencies<'hir>(
    def: &hir::Def,
    local_env: &mut LocalEnv,
    item_env: &ItemEnv<'hir>,
    required_items: &mut ItemSet<'hir>,
) {
    let hir::Def { r#type, expr, .. } = def;
    if let Some(r#type) = r#type {
        expr_dependencies(r#type, local_env, item_env, required_items);
    }
    expr_dependencies(expr, local_env, item_env, required_items);
}

fn expr_dependencies<'hir>(
    expr: &hir::Expr,
    local_env: &mut LocalEnv,
    item_env: &ItemEnv<'hir>,
    required_items: &mut ItemSet<'hir>,
) {
    match expr {
        hir::Expr::Ident(.., ident) => {
            ident_expr_dependencies(*ident, local_env, item_env, required_items);
        }
        hir::Expr::Let(.., (pat, r#type, init, body)) => {
            if let Some(r#type) = r#type {
                expr_dependencies(r#type, local_env, item_env, required_items);
            }
            expr_dependencies(init, local_env, item_env, required_items);
            let len = local_env.len();
            push_pat_names(pat, local_env);
            expr_dependencies(body, local_env, item_env, required_items);
            local_env.truncate(len);
        }
        hir::Expr::RecordType(.., fields) => {
            let len = local_env.len();
            for field in *fields {
                expr_dependencies(&field.r#type, local_env, item_env, required_items);
                local_env.push(field.name.symbol);
            }
            local_env.truncate(len);
        }
        hir::Expr::FunType(.., params, body) | hir::Expr::FunLit(.., params, body) => {
            let len = local_env.len();
            for param in *params {
                push_pat_names(&param.pat, local_env);
                if let Some(r#type) = param.r#type {
                    expr_dependencies(&r#type, local_env, item_env, required_items);
                }
            }
            expr_dependencies(body, local_env, item_env, required_items);
            local_env.truncate(len);
        }
        hir::Expr::RecordLit(.., fields) => {
            fields.iter().for_each(|field| match field.expr.as_ref() {
                Some(expr) => expr_dependencies(expr, local_env, item_env, required_items),
                None => {
                    ident_expr_dependencies(field.name, local_env, item_env, required_items);
                }
            });
        }
        hir::Expr::MethodCall(.., target, method, args) => {
            expr_dependencies(target, local_env, item_env, required_items);
            ident_expr_dependencies(*method, local_env, item_env, required_items);
            args.iter().for_each(|arg| {
                expr_dependencies(&arg.expr, local_env, item_env, required_items);
            });
        }
        hir::Expr::Match(.., scrut, cases) => {
            expr_dependencies(scrut, local_env, item_env, required_items);
            for case in *cases {
                let hir::MatchCase { pat, guard, expr } = case;

                let len = local_env.len();
                push_pat_names(pat, local_env);
                if let Some(guard) = guard {
                    expr_dependencies(guard, local_env, item_env, required_items);
                }
                expr_dependencies(expr, local_env, item_env, required_items);
                local_env.truncate(len);
            }
        }
        _ => expr.for_each_shallow(
            |expr| expr_dependencies(expr, local_env, item_env, required_items),
            |_| (),
        ),
    }
}

fn ident_expr_dependencies<'hir>(
    ident: Ident,
    local_env: &LocalEnv,
    item_env: &ItemEnv<'hir>,
    required_items: &mut ItemSet<'hir>,
) {
    if !local_env.contains(&ident.symbol) {
        if let Some(item) = item_env.get(&ident.symbol) {
            required_items.insert(Identity(item));
        }
    }
}

fn push_pat_names(pat: &hir::Pat, local_env: &mut LocalEnv) {
    match pat {
        hir::Pat::Error(_) | hir::Pat::Lit(..) | hir::Pat::Underscore(_) => {}
        hir::Pat::Ident(.., ident) => local_env.push(ident.symbol),
        hir::Pat::Or(.., pats) => {
            pats.iter().for_each(|pat| push_pat_names(pat, local_env));
        }
        hir::Pat::TupleLit(.., pats) => {
            pats.iter().for_each(|pat| push_pat_names(pat, local_env));
        }
        hir::Pat::RecordLit(.., fields) => fields.iter().for_each(|field| match field.pat {
            Some(pat) => push_pat_names(&pat, local_env),
            None => local_env.push(field.name.symbol),
        }),
    }
}
