#include <stdlib.h>
#include "HsFFI.h"

// #ifndef GLASGOW_HASKELL
// // definitions to fix intellisense warnings when HsFFI.h cannot be found
// #define HS_BOOL_TRUE 1U
// typedef unsigned char HsBool;
// extern HsBool hs_init(int *argc, char ***argv);
// extern void hs_exit(void);
// #endif // GLASGOW_HASKELL

// #define USER_SPECIFIED_INIT
// Uncomment this and define as a function with the signature: HsBool absoluteSolverInit(void) 
// to use a custom initialization function for the module.

HsBool absoluteSolverInit(void)
{
    static int argc         = 1;
    static char *argv_[]    = { "mod_init.so", 0 };
    static char **argv      = argv_;
    (void)hs_init(&argc, &argv);
#ifdef USER_SPECIFIED_INIT
    if (HS_BOOL_TRUE != USER_SPECIFIED_INIT()) 
    {
        hs_exit();
    }
#else
    return HS_BOOL_TRUE;
#endif // USER_SPECIFIED_INIT
}

void absoluteSolverExit(void)
{
    hs_exit();
}