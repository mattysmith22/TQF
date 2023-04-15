Types_Narrowing_fnc_testNarrowing2 = {
    params ["_x"];
    if (_x isEqualType "" || _x isEqualType 1.0) then {
        str _x;
        systemChat str _x;
    } else {
        systemChat "Well we got a hashmap so I can't systemChat it";
    };
};
Types_Narrowing_fnc_testNarrowing = {
    params ["_x"];
    if (_x isEqualType "") then {
        systemChat _x;
    } else {
        systemChat "Well we got a number so I can't systemChat it";
    };
};