Statement_Conditional_fnc_testIfAsExpr = {
    params [];
    private _x = if (1.0 isEqualTo 2.0) then {
        "true";
    } else {
        "false";
    };
    nil;
};
Statement_Conditional_fnc_testExitWith = {
    params [];
    if (1.0 isEqualTo 2.0) exitWith {
        nil;
    };
    nil;
};
Statement_Conditional_fnc_testIfElse = {
    params [];
    if (1.0 isEqualTo 2.0) then {
        systemChat "True";
    } else {
        systemChat "False";
    };
    nil;
};
Statement_Conditional_fnc_testIfNoElse = {
    params [];
    if (1.0 isEqualTo 2.0) then {
        systemChat "True";
    };
    nil;
};