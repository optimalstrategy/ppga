//! This module defines the [`PPGAConfig`] struct used to configure the transpiler.
//!
//! [`PPGAConfig`]: crate::config::PPGAConfig
use crate::codegen::code_builder::DEFAULT_INDENT_SIZE;

/// The config used by the transpiler.
#[derive(Debug, Clone)]
pub struct PPGAConfig {
    /// If `true`, the transpiler will emit the comments
    /// from the original `.ppga` file into the resulting `.lua` file.
    pub emit_comments: bool,
    /// The number of indentation spaces in the resulting lua code.
    pub indent_size: usize,
    /// Specifies whether to include PPGA's standard library symbols.
    /// They are required for some of PPGA's features to work (for example, the `??` and `?` operators).
    pub include_ppga_std: bool,
    /// The default error message returned by the implicit err block.
    /// If the message is `$err`, the `err` variable will be returned.
    pub err_block_default_message: String,
    /// The function used to log the errors handled by the implicit err block.
    /// Defaults to a no-op.
    pub err_block_logger: String,
}

impl Default for PPGAConfig {
    /// Creates a [`PPGAConfig`] with an ident size of [`DEFAULT_INDENT_SIZE`], comments turned off,
    /// and standard symbols included.
    ///
    /// [`PPGAConfig`]: crate::config::PPGAConfig
    /// [`DEFAULT_INDENT_SIZE`]: crate::codegen::code_builder::DEFAULT_INDENT_SIZE
    fn default() -> Self {
        Self {
            emit_comments: false,
            indent_size: DEFAULT_INDENT_SIZE,
            include_ppga_std: true,
            err_block_default_message: String::from("$err"),
            err_block_logger: String::from("(function(_)return end)"),
        }
    }
}

impl PPGAConfig {
    /// Sets `include_ppga_std` to `false`.
    pub fn disable_std(self) -> Self {
        Self {
            include_ppga_std: false,
            ..self
        }
    }

    pub fn custom_err_message(self, msg: String) -> Self {
        Self {
            err_block_default_message: msg,
            ..self
        }
    }
}
