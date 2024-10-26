% Wumpus World Adventure Game
% A text-based adventure game implemented in Prolog
% Modified from the original Nani Search game by Amzi! inc. and by Dr. Cliburn
% Authors: Sarah Akhtar and Kieran Monks
% Date: October 26th, 2024

main:- wumpus_world.

wumpus_world:-
  write('WUMPUS WORLD - A Text Adventure Game'),nl,
  write('Your quest is to slay the Wumpus!'),nl,
  nl,
  write('Commands you can use:'),nl,
  write('  look.              - Look around'),nl,
  write('  inventory.         - Check your possessions'),nl,
  write('  talk to person.    - Talk to someone'),nl,
  write('  go to place.       - Travel to a location'),nl,
  write('  fight monster.     - Battle a monster'),nl,
  write('  buy item.          - Purchase an item'),nl,
  write('  sell item.         - Sell an item'),nl,
  write('  take item.         - Pick up an item'),nl,
  write('  ride transport.    - Use transportation'),nl,
  write('  light item.        - Light something'),nl,
  nl,
  write('Begin your adventure! Type "look." to start.'),nl,
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
door(river, dock).  % Added connection for boat travel
door(dock, town).   % Added connection back to town

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

% Initial state
:- dynamic location/2.
:- dynamic here/1.
:- dynamic have/1.
:- dynamic money/1.
:- dynamic lit/1.
:- dynamic defeated/1.

% Initial facts
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
  asserta(location(archer, canyon)),
  asserta(location(fisherman, dock)),
  asserta(location(blacksmith, shop)),
  asserta(location(troll, mountain)),
  asserta(location(wumpus, forest)),
  asserta(location(boat, river)),
  asserta(location(diamond, river)),
  asserta(money(0)).

% Command processing
command_loop:-
  repeat,
  get_command(X),
  do(X),
  (victory; X == quit).

% Victory condition
victory:-
  defeated(wumpus),
  write('Congratulations! You have slain the Wumpus and won the game!'),nl.

% Command handlers
do(go(Place)):- goto(Place), !.
do(look):- look, !.
do(inventory):- inventory, !.
do(talk(Person)):- talk(Person), !.
do(fight(Monster)):- fight(Monster), !.
do(buy(Item)):- buy(Item), !.
do(sell(Item)):- sell(Item), !.
do(take(Item)):- take(Item), !.
do(ride(Transport)):- ride(Transport), !.
do(light(Item)):- light(Item), !.
do(quit):- quit, !.
do(_):- write('I don\'t understand that command.'), nl.

% Movement logic
goto(Place):-
  can_go(Place),
  check_conditions(Place),
  moveto(Place),
  write('You moved to '), write(Place), write('!'),nl.
goto(_):- nl.

can_go(Place):-
  here(Here),
  connect(Here,Place),!.
can_go(Place):-
  write('You can\'t get to '), write(Place), write(' from here.'), nl,
  write('(Maybe you are already here!)'),
  fail.

connect(X,Y):- door(X,Y).
connect(X,Y):- door(Y,X).

check_conditions(river) :-
  lit(hay), !.
check_conditions(river) :-
  write('It is too dark to enter, you need some light.'),
  !, fail.
check_conditions(_).

moveto(Place):-
  retract(here(_)),
  asserta(here(Place)).

% Look around
look:-
  here(Here),
  write('You are at the '), write(Here), nl,
  write('You can see:'), nl,
  list_visible(Here),
  write('You can go to:'), nl,
  list_connections(Here).

% List all visible items in the current location
list_visible(Place) :-
    findall(X, location(X, Place), Items),
    (Items = [] -> tab(2), write('nothing'), nl;
     list_items(Items)).

% Helper predicate to list items
list_items([]) :- !.
list_items([Item|Rest]) :-
    tab(2), write(Item), nl,
    list_items(Rest).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

% Inventory management with improved formatting
inventory:-
  write('You have:'), nl,
  list_possessions,
  money(M),
  write('Money: '), write(M), write(' coins'), nl.

%% Inventory Management
list_possessions :-
    findall(X, have(X), Possessions),
    (Possessions = [] -> tab(2),write('nothing'), nl;
     list_items(Possessions)).

% Item manipulation with fixed handling
take(Item):-
  here(Here),
  location(Item,Here),
  retract(location(Item,Here)),
  asserta(have(Item)),
  write('You took the '), write(Item), write('!'), 
  nl, !.
take(Item):-
  write('There is no '), write(Item), write(' here.'), 
  nl.
                                                    
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
fight(Monster) :-
    monster(Monster),  % Check if it's a valid monster
    here(Here),
    location(Monster, Here),
    can_fight(Monster),
    handle_victory(Monster), !.
fight(Monster) :-
    monster(Monster),  % Check if it's a valid monster
    here(Here),
    location(Monster, Here),
    write('You cannot defeat the '), write(Monster),
    write(' without proper equipment!'), nl, !.
fight(Monster) :-
    monster(Monster),  % Check if it's a valid monster
    write('There is no '), write(Monster), write(' here.'), nl, !.
fight(Entity) :-
    write('You cannot fight '), write(Entity), write('. It is not a valid enemy.'), nl.

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

% Buy an item
buy(sword) :-
    here(shop),
    money(M),
    M >= 100,
    retract(money(M)),
    NewM is M - 100,
    asserta(money(NewM)),
    asserta(have(sword)),
    write('You bought the sword for 100 coins.'), 
    nl, !.
buy(Item) :-
    item(Item),
    write('You cannot buy the '), write(Item), write(' here.'), 
    nl, !.
buy(Item) :-
    write('You cannot buy that type of item. '), write(Item), write(' is not a valid item.'),
    nl.

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
  write('You don\'t have a diamond to trade.'),
  nl, nl, !.
sell(Item):-
  nl,
  write('You cannot sell the '), write(Item), write(' here.'),
  nl, nl.
                                                            
ride(boat):-
  here(river),
  have(diamond),  % If they have the diamond, they can use the boat
  moveto(dock),
  write('You ride the boat to the dock.'),nl,
  look, !.
ride(boat):-
  here(river),
  \+ have(diamond),
  write('You need to take the diamond first before you can use the boat.'),nl, !.
ride(Transport):-
  write('You cannot ride the '), write(Transport), write(' here.'),nl.
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
  write('I don\'t understand, please try again.'), nl,
  fail.

command(C,L) :- elim(NL,L), C =.. NL, !.

elim([talk,X], [talk,to,the,X,'.']) :- !.
elim([talk,X], [talk,to,X,'.']) :- !.
elim(NL, [V,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,to,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,'.']) :- NL = [V], !.

% Initialize game when loaded
:- init_game.
