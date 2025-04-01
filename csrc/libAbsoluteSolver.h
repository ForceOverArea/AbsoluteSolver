#ifndef LIBABSOLUTESOLVER_H_
#define LIBABSOLUTESOLVER_H_
#include <stdlib.h>
#include "HsFFI.h"

// Module initialization functions

/**
 * \brief Initializes the Haskell RTS for functions in the AbsoluteSolver library.
 *        This must be called before any other function in the library.
 *
 * \returns a boolean value indicating if the RTS was initialized properly.
 */
HsBool absoluteSolverInit(void);

/**
 * \brief Cleans up the Haskell RTS for functions in the AbsoluteSolver library.
 *        This *should* be called ONCE after the AbsoluteSolver library is done 
 *        being used. 
 */
void absoluteSolverExit(void);

// Actual Haskell module FFI exports

/**
 * \brief Symbolically solves for the given variable `targetVar` in the
 *        given equation `eqn`. The returned string pointer must be 
 *        deallocated by the calling code.
 *
 * \param[in] eqn the equation to symbolically solve by isolation
 * \param[in] targetVar the variable to solve for in `eqn`
 * \returns the equation symbolically solved for `targetVar` or an error message.
 */
char *solvedForHs(char *eqn, char *targetVar);

/**
 * \brief Symbolically solves for the given variable `targetVar` in the
 *        given equation `eqn`, then evaluates the expression for the value
 *        of `targetVar`. This is useful for single-instance single-unknown 
 *        problems that must be solved to high accuracy, as this function 
 *        does not iteratively solve for the given variable to a given 
 *        accuracy.
 *
 * \param[in] eqn the equation to symbolically solve by isolation
 * \param[in] targetVar the variable to solve for in `eqn`
 * \param[in] constVals the context containing constants and their values 
 *            in `eqn`. Format is "var1=10,var2=9.0,..."
 * \returns the value of `targetVar` or NaN if an error occurred.
 */
double solvedForValueHs(char *eqn, char *targetVar, char *constVals);

#endif // LIBABSOLUTESOLVER_H_