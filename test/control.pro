:- module(test_control, []).
:- use_module('src/prolog/promini.pro').
:- use_module(library(plunit)).

:- begin_tests(control).

test(control_init, [nondet]) :-
    control_init.

test(control_init_idempotent, [nondet]) :-
    control_init,
    control_init.

test(control_shutdown, [nondet]) :-
    control_init,
    control_shutdown.

test(control_shutdown_idempotent, [nondet]) :-
    control_init,
    control_shutdown,
    control_shutdown.

test(control_gamepads, [nondet, setup(control_init), cleanup(control_shutdown)]) :-
    control_gamepads([_|_]).

test(control_open_close, [nondet, setup(control_init), cleanup(control_shutdown)]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, Handle),
    control_close(Handle).

test(control_axis, [nondet, setup(control_init), cleanup(control_shutdown)]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, Handle),
    control_axis(Handle, left_x, Value),
    Value >= -1.0,
    Value =< 1.0,
    control_close(Handle).

test(control_axis_dpad_x, [nondet, setup(control_init), cleanup(control_shutdown)]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, Handle),
    control_axis(Handle, dpad_x, Value),
    Value >= -1.0,
    Value =< 1.0,
    control_close(Handle).

test(control_axis_dpad_y, [nondet, setup(control_init), cleanup(control_shutdown)]) :-
    control_gamepads([gamepad(Id, _)|_]),
    control_open(Id, Handle),
    control_axis(Handle, dpad_y, Value),
    Value >= -1.0,
    Value =< 1.0,
    control_close(Handle).

:- end_tests(control).
