% Wumpus World Adventure Game
% A text-based adventure game implemented in Prolog
% Modified from the original Nani Search game by Amzi! inc.

% ANSI color codes
color_code(red,    '\u001b[31m').
color_code(green,  '\u001b[32m').
color_code(yellow, '\u001b[33m').
color_code(blue,   '\u001b[34m').
color_code(reset,  '\u001b[0m').

% Color output helper predicate
write_colored(Color, Text) :-
    color_code(Color, Code),
    color_code(reset, Reset),
    format('~w~w~w', [Code, Text, Reset]).

main :- wumpus_world.

wumpus_world :-
    write_colored(yellow, 'WUMPUS WORLD - A Text Adventure Game\n'),
    write('Your quest is to slay the fearsome Wumpus!\n\n'),
    write_colored(green, 'Commands you can use:\n'),
    write('  look.              - Look around\n'),
    write('  inventory.         - Check your possessions\n'),
    write('  talk to person.    - Talk to someone\n'),
    write('  go to place.       - Travel to a location\n'),
    write('  fight monster.     - Battle a monster\n'),
    write('  buy item.          - Purchase an item\n'),
    write('  sell item.         - Sell an item\n'),
    write('  take item.         - Pick up an item\n'),
    write('  ride transport.    - Use transportation\n'),
    write('  light item.        - Light something\n\n'),
    write_colored(blue, 'Begin your adventure! Type "look." to start.\n'),
    command_loop.

% Rooms/locations
room(prairie).
room(foothills).
room(town).
room(woods).
room(sinkhole).
room(cave).
room(river).
room(dock).
room(shop).
room(mountain).
room(canyon).
room(forest).

% Connections between locations
door(prairie, foothills).
door(prairie, sinkhole).
door(prairie, woods).
door(prairie, canyon).
door(foothills, town).
door(foothills, mountain).
door(town, woods).
door(town, shop).
door(woods, forest).
door(sinkhole, cave).
door(cave, river).
door(river, dock).
door(dock, town).

% NPCs and monsters
npc(archer).
npc(fisherman).
npc(blacksmith).
monster(troll).
monster(wumpus).

% Items that can be picked up
item(matches).
item(hay).
item(diamond).
item(sword).
item(arrow).
item(bow).
item(boat).

% Dynamic predicates
:- dynamic location/2.
:- dynamic here/1.
:- dynamic have/1.
:- dynamic money/1.
:- dynamic lit/1.
:- dynamic defeated/1.

% Initial game state
init_game :-
    retractall(location(_, _)),
    retractall(here(_)),
    retractall(have(_)),
    retractall(money(_)),
    retractall(lit(_)),
    retractall(defeated(_)),
    asserta(here(prairie)),
    asserta(location(matches, foothills)),
    asserta(location(hay, woods)),
    asserta(location(diamond, river)),
    asserta(location(archer, canyon)),
    asserta(location(fisherman, dock)),
    asserta(location(blacksmith, shop)),
    asserta(location(troll, mountain)),
    asserta(location(wumpus, forest)),
    asserta(location(boat, river)),
    asserta(money(0)).

% Command processing
command_loop :-
    repeat,
    get_command(X),
    do(X),
    (victory; X == quit).

% Victory condition
victory :-
    defeated(wumpus),
    write_colored(green, 'Congratulations! You have slain the Wumpus and won the game!\n').

% Command handlers
do(go(Place)) :- goto(Place), !.
do(look) :- look, !.
do(inventory) :- inventory, !.
do(talk(Person)) :- talk(Person), !.
do(fight(Monster)) :- fight(Monster), !.
do(buy(Item)) :- buy(Item), !.
do(sell(Item)) :- sell(Item), !.
do(take(Item)) :- take(Item), !.
do(ride(Transport)) :- ride(Transport), !.
do(light(Item)) :- light(Item), !.
do(quit) :- quit, !.
do(_) :- write_colored(red, 'I don\'t understand that command.\n').

% Movement logic with river visibility
goto(Place) :-
    can_go(Place),
    check_conditions(Place),
    moveto(Place),
    look.
goto(_) :- look.

can_go(Place) :-
    here(Here),
    connect(Here, Place),
    visible_location(Place), !.
can_go(Place) :-
    write_colored(red, 'You can\'t get to '),
    write(Place),
    write(' from here.\n'),
    fail.

% Location visibility check
visible_location(river) :-
    lit(hay), !.
visible_location(Place) :-
    Place \= river.

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

check_conditions(river) :-
    lit(hay), !.
check_conditions(river) :-
    write_colored(red, 'It\'s too dark to enter. You need some light.\n'),
    !, fail.
check_conditions(_).

moveto(Place) :-
    retract(here(_)),
    asserta(here(Place)).

% Look around with improved visibility
look :-
    here(Here),
    write('You are at the '),
    write_colored(green, Here),
    nl,
    write_colored(yellow, 'You can see:\n'),
    list_visible(Here),
    write_colored(yellow, 'You can go to:\n'),
    list_connections(Here).

list_visible(Place) :-
    location(X, Place),
    (visible_location(Place) -> 
        write('  '),
        write_colored(blue, X),
        nl
    ; true),
    fail.
list_visible(_).

list_connections(Place) :-
    connect(Place, X),
    (visible_location(X) ->
        write('  '),
        write_colored(green, X),
        nl
    ; true),
    fail.
list_connections(_).

% Inventory management
inventory :-
    nl,
    write_colored(yellow, 'You have:\n'),
    list_possessions,
    nl,
    money(M),
    write('Money: '),
    write_colored(yellow, M),
    write(' coins\n'),
    nl.

list_possessions :-
    have(X),
    write('  '),
    write_colored(blue, X),
    nl,
    fail.
list_possessions.

% Item manipulation
take(Item) :-
    here(Here),
    location(Item, Here),
    retract(location(Item, Here)),
    asserta(have(Item)),
    nl,
    write('You took the '),
    write_colored(green, Item),
    nl, nl, !.
take(Item) :-
    nl,
    write('There is no '),
    write_colored(red, Item),
    write(' here.'),
    nl, nl.
                                                    
% NPC interaction
talk(Person):-
  here(Here),
  location(Person,Here),
  give_hint(Person), !.
talk(Person):-
  write(Person), write(' is not here.'), nl.

give_hint(archer):-
  write('The archer says: "I would trade my bow for a diamond!"'), nl.
give_hint(fisherman):-
  write('The fisherman says: "I would give you money for a boat!"'), nl.
give_hint(blacksmith):-
  write('The blacksmith says: "I will sell you a sword if you have money!"'), nl.

% Combat system
fight(Monster):-
  here(Here),
  location(Monster,Here),
  can_fight(Monster),
  handle_victory(Monster), !.
fight(Monster):-
  here(Here),
  location(Monster,Here),
  write('You cannot defeat the '), write(Monster),
  write(' without proper equipment!'), nl, !.
fight(Monster):-
  write('There is no '), write(Monster), write(' here.'), nl.

can_fight(troll):- have(sword), !.
can_fight(wumpus):- have(bow), have(arrow), !.
can_fight(_):- fail.

handle_victory(troll):-
  retract(location(troll,mountain)),
  asserta(location(arrow,mountain)),
  asserta(defeated(troll)),
  write('You defeated the troll! An arrow appears!'), nl.
handle_victory(wumpus):-
  asserta(defeated(wumpus)),
  write('You have slain the Wumpus!'), nl.

% Trading system with fixed item handling
buy(sword):-
  here(shop),
  money(M),
  M >= 100,
  retract(money(M)),
  NewM is M - 100,
  asserta(money(NewM)),
  asserta(have(sword)),
  nl,
  write('You bought the sword for 100 coins.'), 
  nl, nl, !.
buy(Item):-
  nl,
  write('You cannot buy the '), write(Item), write(' here.'), 
  nl, nl.

sell(boat):-
  here(dock),
  have(diamond),  % If they have the diamond, they can sell the boat
  money(M),
  NewM is M + 100,
  retract(money(M)),
  asserta(money(NewM)),
  nl,
  write('You sold the boat for 100 coins.'),
  nl, nl, !.
sell(boat):-
  here(dock),
  \+ have(diamond),
  nl,
  write('You need to get the diamond before you can sell the boat.'),
  nl, nl, !.
sell(diamond):-
  here(canyon),
  have(diamond),
  retract(have(diamond)),
  asserta(have(bow)),
  nl,
  write('The archer trades you a bow for the diamond.'),
  nl, nl, !.
sell(diamond):-
  here(canyon),
  \+ have(diamond),
  nl,
  write('You do not have a diamond to trade.'),
  nl, nl, !.
sell(Item):-
  nl,
  write('You cannot sell the '), write(Item), write(' here.'),
  nl, nl.
                                                            
ride(boat):-
  here(river),
  have(diamond),  % If they have the diamond, they can use the boat
  moveto(dock),
  nl,
  write('You ride the boat to the dock.'), 
  nl, nl,
  look, !.
ride(boat):-
  here(river),
  \+ have(diamond),
  nl,
  write('You need to take the diamond first before you can use the boat.'),
  nl, nl, !.
ride(Transport):-
  nl,
  write('You cannot ride the '), write(Transport), write(' here.'),
  nl, nl.

ride(Transport):-
  write('You cannot ride the '), write(Transport), write(' here.'), nl.

% Lighting system
light(hay):-
  have(hay),
  have(matches),
  asserta(lit(hay)),
  write('You light the hay, illuminating the darkness.'), nl, !.
light(Item):-
  write('You cannot light the '), write(Item), write('.'), nl.

% Quit game
quit:-
  write('Thanks for playing!'), nl.

% Command parsing
get_command(C) :- 
  read_line_to_string(user_input, String),
  tokenize_atom(String, L),
  command(C,L), !.
get_command(_) :- 
  write('I do not understand, please try again.'), nl,
  fail.

command(C,L) :- elim(NL,L), C =.. NL, !.

elim([talk,X], [talk,to,the,X,'.']) :- !.
elim([talk,X], [talk,to,X,'.']) :- !.
elim(NL, [V,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,to,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,'.']) :- NL = [V], !.

% Initialize game when loaded
:- init_game.
