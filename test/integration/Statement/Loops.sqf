Statement_Loops_fnc_loopAsExpr = {
    params [];
    while {
        true;
    } do {
        1.0;
    };
};
Statement_Loops_fnc_normalExitWith = {
    params [];
    while {
        true;
    } do {
        systemChat "test";
        if true exitWith {
            1.0;
        };
    };
    nil;
};
Statement_Loops_fnc_normalWhileLoop = {
    params [];
    while {
        true;
    } do {
        systemChat "test";
    };
};