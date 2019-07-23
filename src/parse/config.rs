use serde::de;

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Mode {
    Replace,
    Protected,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub mode: Mode,
    pub statement_line_open: String,
    pub statement_block_open: String,
    pub statement_block_close: String,
    pub script_block_open: String,
    pub script_block_close: String,
    pub directive_open: String,
    pub directive_formal_open: String,
    pub directive_formal_close: String,
    pub block_open: String,
    pub block_close: String,
    pub interpolation_open: String,
    pub interpolation_close: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            mode: Mode::Replace,
            statement_line_open: "@@".to_string(),
            statement_block_open: "<@".to_string(),
            statement_block_close: "@>".to_string(),
            script_block_open: "<{".to_string(),
            script_block_close: "}>".to_string(),
            directive_open: "#".to_string(),
            directive_formal_open: "#{".to_string(),
            directive_formal_close: "}".to_string(),
            block_open: "{{:".to_string(),
            block_close: "}}".to_string(),
            interpolation_open: "<%".to_string(),
            interpolation_close: "%>".to_string(),
        }
    }
}

struct ConfigDef<'a>(&'a mut Config);

impl<'a, 'de> de::DeserializeSeed<'de> for ConfigDef<'a> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        const FIELDS: &'static [&'static str] = &[
            "mode",
            "statement_line_open",
            "statement_block_open",
            "statement_block_close",
            "script_block_open",
            "script_block_close",
            "directive_open",
            "directive_formal_open",
            "directive_formal_close",
            "block_open",
            "block_close",
            "interpolation_open",
            "interpolation_close",
        ];
        deserializer.deserialize_struct("Config", FIELDS, ConfigDefVisitor(self))
    }
}

struct ConfigDefVisitor<'a>(ConfigDef<'a>);

impl<'a, 'de> de::Visitor<'de> for ConfigDefVisitor<'a> {
    type Value = ();

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "struct Config")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let c = (self.0).0;
        while let Some(k) = map.next_key()? {
            match k {
                "mode" => c.mode = map.next_value()?,
                "statement_line_open" => c.statement_line_open = map.next_value()?,
                "statement_block_open" => c.statement_block_open = map.next_value()?,
                "statement_block_close" => c.statement_block_open = map.next_value()?,
                "script_block_open" => c.script_block_open = map.next_value()?,
                "script_block_close" => c.script_block_close = map.next_value()?,
                "directive_open" => c.directive_open = map.next_value()?,
                "directive_formal_open" => c.directive_formal_open = map.next_value()?,
                "directive_formal_close" => c.directive_formal_close = map.next_value()?,
                "block_open" => c.block_open = map.next_value()?,
                "block_close" => c.block_close = map.next_value()?,
                "interpolation_open" => c.interpolation_open = map.next_value()?,
                "interpolation_close" => c.interpolation_close = map.next_value()?,
                _ => unimplemented!(),
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_with_seed() {
        use serde::de::DeserializeSeed;

        let mut c = Config::default();

        assert_eq!(c.mode, Mode::Replace);
        assert_eq!(c.statement_line_open, "@@");

        let s = "mode = 'protected', statement_line_open = '@@@'".replace(',', "\n");

        let mut p = toml::Deserializer::new(&s);
        ConfigDef(&mut c).deserialize(&mut p).unwrap();

        assert_eq!(c.mode, Mode::Protected);
        assert_eq!(c.statement_line_open, "@@@");
    }
}
