Types_Generics_fnc_mainInferred = {
    params [];
    private _array1 = [1.0, 2.0, 3.0];
    systemChat str count _array1;
};
Types_Generics_fnc_countOfSomethingInferred = {
    params ["_x"];
    count (_x get "theArray");
};
Types_Generics_fnc_main = {
    params [];
    private _array1 = [1.0, 2.0, 3.0];
    systemChat str count _array1;
};
Types_Generics_fnc_countOfSomething = {
    params ["_x"];
    count (_x get "theArray");
};