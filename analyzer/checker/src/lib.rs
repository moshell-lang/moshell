mod define_std;

use analyzer_system::layers::ModuleLayers;
use ast::AST;

pub fn checker(files: Vec<AST>) -> Result<(), String> {
    let layers = ModuleLayers::rc_new();
    
}

#[cfg(test)]
mod tests {

}
