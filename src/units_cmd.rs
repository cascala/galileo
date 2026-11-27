//! Commands for units: unit(3, "m/s") (what 3 m/s means), convert(q, km/h),
//! convert(q) to SI base units, dimension(q), value(q, unit).

use crate::eval::{R, Session};
use crate::expr::Expr;
use crate::parser::Node;
use crate::units;

impl Session {
    pub(crate) fn units_call(&mut self, f: &str, args: &[Node]) -> Option<R<Expr>> {
        let r = match f {
            "unit" => self.unit_cmd(args),
            "convert" => self.convert_cmd(args),
            "value" => self.value_cmd(args),
            "dimension" => match args {
                [q] => self.eval(q).and_then(|q| units::dimension(&q).map(Expr::Str)),
                _ => Err("usage: dimension(q), such as dimension(3 m/s)".into()),
            },
            _ => return None,
        };
        Some(r)
    }

    fn unit_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let usage = "usage: unit(x, \"m/s\") is x m/s";
        let (x, u) = match args {
            [x, u] => (self.eval(x)?, self.eval(u)?),
            [u] => (Expr::one(), self.eval(u)?),
            _ => return Err(usage.into()),
        };
        let Expr::Str(u) = u else { return Err(usage.into()) };
        let u = units::expr_of(&units::parse(&u)?);
        crate::eval::arith(crate::parser::Op::Mul, x, u)
    }

    /// value(q): the number in SI base units; value(q, km/h): in those units
    fn value_cmd(&mut self, args: &[Node]) -> R<Expr> {
        let (q, t) = match args {
            [q] => (self.eval(q)?, None),
            [q, t] => (self.eval(q)?, Some(self.eval(t)?)),
            _ => return Err("usage: value(q) is the number in SI units, value(q, km/h) in other units".into()),
        };
        let c = units::convert(&q, t.as_ref())?;
        Ok(units::strip(&c))
    }

    fn convert_cmd(&mut self, args: &[Node]) -> R<Expr> {
        match args {
            [q] => units::convert(&self.eval(q)?, None),
            [q, t] => {
                let q = self.eval(q)?;
                let t = self.eval(t)?;
                units::convert(&q, Some(&t))
            }
            _ => Err("usage: convert(q, km/h) changes units; convert(q) gives SI base units".into()),
        }
    }
}
