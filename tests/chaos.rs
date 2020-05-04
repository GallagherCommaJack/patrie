#![allow(warnings)]

use patrie::Trie;
use proptest::prelude::*;
use proptest_derive::*;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Arbitrary)]
enum TableOp {
    Insert(Vec<u8>, u64),
    Remove(Vec<u8>),
    Get(Vec<u8>),
}

struct Models {
    trie: Trie<u64>,
    model: BTreeMap<Vec<u8>, u64>,
}

impl Models {
    fn new() -> Self {
        let trie = Trie::new();
        let model = BTreeMap::new();
        Self { trie, model }
    }

    fn run_op(&mut self, op: TableOp) {
        match op {
            TableOp::Insert(key, val) => {
                let tr = self.trie.insert(&key, val.clone());
                let mo = self.model.insert(key, val);
                assert_eq!(tr, mo);
            }
            TableOp::Remove(key) => {
                let tr = self.trie.remove(&key);
                let mo = self.model.remove(&key);
                assert_eq!(tr, mo);
            }
            TableOp::Get(key) => {
                let tr = self.trie.get(&key);
                let mo = self.model.get(&key);
                assert_eq!(tr, mo);
            }
        }
    }

    fn validate(&self) {
        // dbg!(&self.trie);
        for (k, v) in self.model.iter() {
            assert_eq!(Some(v), self.trie.get(&k))
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100_000))]
    #[test]
    fn always_equiv(ops: Vec<TableOp>) {
        let mut model = Models::new();
        for op in ops {
            model.run_op(op);
        }
        model.validate();
    }
}
