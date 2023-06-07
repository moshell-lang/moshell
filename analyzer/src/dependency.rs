use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// A directed graph of dependencies.
#[derive(Debug, Clone, PartialEq)]
pub struct Dependencies<N>
where
    N: Eq + Hash,
{
    /// The nodes of the graph.
    top: HashMap<N, Vec<N>>,
}

impl<N> Dependencies<N>
where
    N: Eq + Hash,
{
    /// Explicitly adds a new node to the graph.
    pub fn add_node(&mut self, node: N) {
        self.top.entry(node).or_default();
    }

    /// Adds a new directed dependency from `from` to `to`.
    ///
    /// If the nodes do not exist, they are implicitly added.
    pub fn add_dependency(&mut self, from: N, to: N) {
        self.top.entry(from).or_default().push(to);
    }
}

impl<N> Default for Dependencies<N>
where
    N: Eq + Hash,
{
    fn default() -> Self {
        Self {
            top: HashMap::new(),
        }
    }
}

/// Gets an ordered list of nodes such that all dependencies are before the node.
///
/// The order is such that if `A` depends on `B`, then `B` will be before `A` in the list.
pub fn topological_sort<N>(dependencies: &Dependencies<N>) -> Vec<N>
where
    N: Eq + Hash + Copy,
{
    let dep_count = dependencies.top.len();
    let mut sorted = Vec::with_capacity(dep_count);
    let mut visited = HashSet::with_capacity(dep_count);
    let mut stack = Vec::new();
    for node in dependencies.top.keys() {
        if visited.contains(node) {
            continue;
        }
        stack.push(*node);
        while let Some(node) = stack.pop() {
            if visited.insert(node) {
                stack.push(node);
                if let Some(dependencies) = dependencies.top.get(&node) {
                    for dependency in dependencies {
                        if !visited.contains(dependency) {
                            stack.push(*dependency);
                        }
                    }
                }
            } else {
                sorted.push(node);
            }
        }
    }
    sorted
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn any_topological_sort() {
        let mut dependencies = Dependencies::default();
        dependencies.add_node(0);
        dependencies.add_node(1);
        dependencies.add_node(2);
        let mut res = topological_sort(&dependencies);
        res.sort();
        assert_eq!(res, vec![0, 1, 2]);
    }

    #[test]
    fn order_topological_sort() {
        let mut dependencies = Dependencies::default();
        dependencies.add_dependency(0, 1);
        dependencies.add_dependency(0, 2);
        dependencies.add_dependency(1, 2);
        dependencies.add_dependency(1, 3);
        dependencies.add_dependency(2, 3);
        let sorted = topological_sort(&dependencies);
        assert_eq!(sorted, vec![3, 2, 1, 0]);
    }
}
