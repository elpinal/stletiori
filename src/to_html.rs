//! This module provides a way to convert a value to an HTML code.

use failure::*;

use super::html::*;
use crate::language::intermediate::dynamic::*;
use crate::language::Lit;

impl Value {
    pub fn into_html(self) -> Fallible<Box<dyn Node>> {
        let sv = SValue::from(self);
        match sv {
            SValue::Vector(v) => {
                let mut iter = v.into_iter();
                let tag = SValue::from(iter.next().ok_or_else(|| err_msg("empty vector"))?)
                    .get_keyword()?;
                if is_void_element(&tag) {
                    Ok(Box::new(VoidElement::new(tag, None)))
                } else {
                    let children: Vec<Box<dyn Node>> =
                        iter.map(|v| v.into_html()).collect::<Result<_, _>>()?;
                    Ok(Box::new(Element::new(tag, None, children)))
                }
            }
            SValue::Lit(Lit::String(s)) => Ok(Box::new(TextNode::new(s))),
            _ => unimplemented!(),
        }
    }
}

impl SValue {
    fn get_keyword(self) -> Fallible<String> {
        match self {
            SValue::Lit(Lit::Keyword(s)) => Ok(s),
            _ => bail!("not keyword: {:?}", self),
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
