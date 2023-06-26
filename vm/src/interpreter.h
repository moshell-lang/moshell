#pragma once

#include "definitions/module_definition.h"


/**
 * Will run given module's main method.
 * @throws InvalidModuleDescription if the given module does not defines a <main>() function
 * @throws InvalidBytecodeError if an interpreted instruction set contains invalid instructions
 * */
void run_module(const module_definition &module_def, strings_t &strings);


