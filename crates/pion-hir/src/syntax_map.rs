use std::collections::hash_map::Iter;

use nohash::IntMap;
use pion_utils::identity::Identity;

#[derive(Debug, Clone)]
pub struct SyntaxMap<'surface, 'hir, Surface, Hir> {
    pub surface_to_hir: IntMap<Identity<&'surface Surface>, &'hir Hir>,
    pub hir_to_surface: IntMap<Identity<&'hir Hir>, &'surface Surface>,
}

impl<'surface, 'hir, Surface, Hir> Default for SyntaxMap<'surface, 'hir, Surface, Hir> {
    fn default() -> Self {
        Self {
            surface_to_hir: IntMap::default(),
            hir_to_surface: IntMap::default(),
        }
    }
}

impl<'surface, 'hir, Surface, Hir> SyntaxMap<'surface, 'hir, Surface, Hir> {
    pub fn new() -> Self { Self::default() }

    pub fn shrink_to_fit(&mut self) {
        self.surface_to_hir.shrink_to_fit();
        self.hir_to_surface.shrink_to_fit();
    }

    pub fn insert(&mut self, surface: &'surface Surface, hir: &'hir Hir) {
        self.surface_to_hir.insert(Identity(surface), hir);
        self.hir_to_surface.insert(Identity(hir), surface);
    }

    pub fn surface(&self, hir: &'hir Hir) -> Option<&'surface Surface> {
        self.hir_to_surface.get(&Identity(hir)).copied()
    }

    pub fn hir(&self, surface: &'surface Surface) -> Option<&'hir Hir> {
        self.surface_to_hir.get(&Identity(surface)).copied()
    }

    pub fn len(&self) -> usize { self.surface_to_hir.len() }

    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn iter(&self) -> Iter<'_, Identity<&'surface Surface>, &'hir Hir> {
        self.surface_to_hir.iter()
    }
}
