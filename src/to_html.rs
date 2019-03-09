//! This module provides a way to convert a value to an HTML code.

use std::collections::BTreeMap;
use std::iter::Peekable;

use failure::*;

use super::html::*;
use crate::language::intermediate::dynamic::*;
use crate::language::Lit;

impl Value {
    pub fn into_html(self) -> Fallible<Box<dyn Node>> {
        let sv = SValue::from(self);
        match sv {
            SValue::Vector(v) => {
                let mut iter = v.into_iter().peekable();
                let tag = SValue::from(iter.next().ok_or_else(|| err_msg("empty vector"))?)
                    .get_keyword()?;
                let attrs = get_attrs(&mut iter)?;
                if is_void_element(&tag) {
                    Ok(Box::new(VoidElement::new(tag, attrs)))
                } else {
                    let children: Vec<Box<dyn Node>> =
                        iter.map(|v| v.into_html()).collect::<Result<_, _>>()?;
                    Ok(Box::new(Element::new(tag, attrs, children)))
                }
            }
            SValue::Lit(Lit::String(s)) => Ok(Box::new(TextNode::new(s))),
            _ => unimplemented!(),
        }
    }
}

fn get_attrs<I>(iter: &mut Peekable<I>) -> Fallible<BTreeMap<String, String>>
where
    I: Iterator<Item = Value>,
{
    match iter.peek() {
        Some(&Value::SValue(SValue::Map(_))) | Some(&Value::UCast(SValue::Map(_))) => {
            let sv: SValue = iter.next().unwrap().into();
            match sv {
                SValue::Map(m) => m
                    .into_iter()
                    .map(|(s, v)| Ok((s, SValue::from(v).get_string()?)))
                    .collect(),
                _ => unreachable!(),
            }
        }
        _ => Ok(BTreeMap::new()),
    }
}

impl SValue {
    fn get_keyword(self) -> Fallible<String> {
        match self {
            SValue::Lit(Lit::Keyword(s)) => Ok(s),
            _ => bail!("not keyword: {:?}", self),
        }
    }

    fn get_string(self) -> Fallible<String> {
        match self {
            SValue::Lit(Lit::String(s)) => Ok(s),
            _ => bail!("not string: {:?}", self),
        }
    }
}

fn is_void_element(s: &str) -> bool {
    match s {
        "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta"
        | "param" | "source" | "track" | "wbr" => true,
        _ => false,
    }
}
