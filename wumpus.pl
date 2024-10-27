% =======================
% WUMPUS WORLD ADVENTURE GAME
% =======================
% 
% This is a text-based adventure game implemented in Prolog.
% Modified from the original Nani Search game by Amzi! inc. and Dr. Cliburn.
% In this game, the player must navigate through various locations, 
% interact with NPCs, gather items, and eventually slay the Wumpus.
%
% Authors: Sarah Akhtar and Kieran Monks
% Date: October 26th, 2024

% =======================
% Main Game Control
% =======================

% Entry point to start the game by invoking wumpus_world
main:- 
    wumpus_world.

% Initializes and begins the Wumpus World adventure game
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
  look,
  command_loop.

% =======================
% Game World Definition
% =======================

% Locations in the wumpus world
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

% Connections between locations (for navigation)
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
door(dock, town).

% NPCs and monsters in Hunt the Wumpus
npc(archer).
npc(fisherman).
npc(blacksmith).
monster(troll).
monster(wumpus).

% Items that can be picked up or used
item(matches).
item(hay).
item(diamond).
item(sword).
item(arrow).
item(bow).
item(boat).

% =======================
% Game State Management
% =======================

% Initial state
:- dynamic location/2.
:- dynamic here/1.
:- dynamic have/1.
:- dynamic money/1.
:- dynamic lit/1.
:- dynamic defeated/1.
:- dynamic traded/1.
traded(none).

% Initialize game state by resetting locations and inventory
init_game :-
  retractall(location(_, _)),
  retractall(here(_)),
  retractall(have(_)),
  retractall(money(_)),
  retractall(lit(_)),
  retractall(defeated(_)),
  retractall(traded(_)),  % Clear any existing trade states
  asserta(traded(none)),  % Initialize trade state
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

% =======================
% Command Processing
% =======================

% Command loop to continuously read and execute player commands
command_loop:-
  repeat,
  get_command(X),
  do(X),
  (victory; X == end).

% Victory condition, triggered when Wumpus is defeated
victory:-
  defeated(wumpus),
  write('Congratulations! You have slain the Wumpus and won the game!'),nl.

% =======================
% Ending the Game
% =======================

% Ends the game session, displaying a farewell message
quit:- 
  not(defeated(wumpus)),
  write('You are not allowed to quit! The Wumpus needs slaying!'), nl.

end:- 
  write('You ended the game! See you again soon.'), nl,
  true.  % Ends the game session

% Command handlers for user specified actions
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
do(end):- end, !.
do(_):- write('I don\'t understand that command.'), nl.

% =======================
% Movement and Navigation
% =======================

% Attempt to move to a specified place and handle all conditions
goto(Place) :-
    room(Place),  % Check if it's a valid room
    here(Here),
    (Here = Place ->
        write('You are already at '), write(Place), write('.'), nl, look;
        can_go(Place) ->
            check_conditions(Place),
            moveto(Place),
            write('You moved to '), write(Place), write('!'), nl, look;
        write('You can\'t get to '), write(Place), write(' from here.'), nl, look
    ).
goto(Place) :-
    \+ room(Place),  % Not a valid room
    write(Place), write(' is not a valid location.'), nl.
goto(_):- nl.

% Check if movement to a place is possible
can_go(Place):-
    here(Here),
    connect(Here,Place), !.

% Define bidirectional connections
connect(X,Y):- door(X,Y).
connect(X,Y):- door(Y,X).

% Check conditions for entering a location
check_conditions(cave) :-
    lit(hay), 
    write('You can now see the river!'), nl,
    !.
check_conditions(river) :-
    lit(hay), !.
check_conditions(river) :-
    write('It is too dark to enter the river. You need some light.'),
    !, fail.
check_conditions(_).

% Move to a new location
moveto(Place) :-
    retract(here(_)),
    asserta(here(Place)).

% =======================
% Look Around Actions
% =======================

% Look around location
look:-
  here(Here),
  write('You are at the '), write(Here), nl,
  write('You can see:'), nl,
  list_visible(Here),
  write('You can go to:'), nl,
  list_connections(Here),
  (Here = cave, not(lit(hay)) -> 
      write('You might need to light something to see more of the map...'), nl
  ; true).

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

% List connected locations
list_connections(Place):-
    connect(Place, X),
    (X = river, not(lit(hay)) -> true; 
     tab(2), write(X), nl),
    fail.
list_connections(_).

% =======================
% Inventory Management
% =======================

% Display player's inventory with formatting
inventory:-
  write('You have:'), nl,
  list_possessions,
  money(M),
  write('Money: '), write(M), write(' coins'), nl.

% Helper to list items in inventory
list_possessions :-
    findall(X, have(X), Possessions),
    (Possessions = [] -> tab(2),write('nothing'), nl;
     list_items(Possessions)).

% Pick up an item if it exists in the current location
take(boat) :-
    here(Here),
    location(boat, Here),
    asserta(have(boat)),
    write('The boat is now in your inventory!'), 
    nl, !.
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

% =======================
% NPC Interaction
% =======================
                                                    
% Talk to an NPC if they are present in the current location
talk(Person):-
  here(Here),
  location(Person,Here),
  response_for_npc(Person), !.
talk(Person):-
  write(Person), write(' is not here.'), nl.

% Define hints or dialogues for each NPC
response_for_npc(wumpus):- 
  write('The Wumpus screams its name angrily, "WUMP. WUMP. WUMP"'), nl.
response_for_npc(troll):- 
  write('The Troll sneers and says, "You won\'t get past me so easily"'), nl.
response_for_npc(fisherman):- 
  (traded(fisherman) ->  % If the player already traded with the fisherman
     write('The fisherman smiles. "Thanks for the boat. Enjoy the rest of your adventure!"'), nl; 
     write('The fisherman tells you that he would give you money for a boat!'), nl
  ).
response_for_npc(blacksmith):- 
  (traded(blacksmith) ->  % If the player already bought the sword
     write('The blacksmith says, "I hope the sword serves you well."'), nl; 
     write('The blacksmith says, "I will sell you a sword if you have money."'), nl
  ).
response_for_npc(archer):- 
  (traded(archer) ->  % If the player already traded the diamond for a bow
     write('The archer says, "Use the bow wisely in your quest."'), nl; 
     write('The archer says, "I would trade my bow for a diamond!"'), nl
  ).

% =======================
% Combat System
% =======================

% Engage in combat with a monster if conditions are met
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

% Define conditions required to fight each type of monster
can_fight(troll):- have(sword), !.
can_fight(wumpus):- have(bow), have(arrow), !.
can_fight(_):- fail.

% Handle events after defeating specific monsters
handle_victory(troll):-
  retract(location(troll,mountain)),
  asserta(location(arrow,mountain)),
  asserta(defeated(troll)),
  write('You defeated the troll! An arrow appears!'), nl.
handle_victory(wumpus):-
  asserta(defeated(wumpus)),
  write('You have slain the Wumpus!'), nl.

% =======================
% Item Transactions
% =======================

% Purchase an item if in the correct location and with enough money
buy(sword) :-
    here(shop),  % Check if the player is at the shop
    money(M),
    M >= 100,  % Check if the player has enough money
    retract(money(M)),
    NewM is M - 100,
    asserta(money(NewM)),
    asserta(have(sword)),
    asserta(traded(blacksmith)),
    write('You bought the sword for 100 coins.'), 
    nl, !.
buy(sword) :-  % If not enough money
    here(shop),
    money(M),
    M < 100,
    write('You do not have sufficient funds to buy the sword!'), nl,
    write('Try selling something you have...'), nl, !.
buy(sword) :-  % If not at the shop
    \+ here(shop),
    write('You cannot buy items unless you are at the shop.'), nl, !.
buy(Item) :-  % If item is invalid
    item(Item),
    write('You cannot buy that type of item. '), write(Item), write(' is not a valid item.'), nl.
buy(Item) :-  % If item is not defined
    \+ item(Item),
    write('You cannot buy a '), write(Item), write('. It is not a valid item.'), nl.

% Sell an item at a specific location
sell(boat) :-
    here(dock),
    have(boat),
    money(M),
    NewM is M + 100,
    retract(money(M)),
    asserta(money(NewM)),
    retract(have(boat)),
    asserta(traded(fisherman)),
    write('You sold the boat for 100 coins.'), nl, !.
sell(boat) :- % Attempt to sell boat without having it
    here(dock),
    \+ have(boat),
    write('You don\'t have a boat to sell.'), nl, !.
sell(boat) :- % Attempt to sell boat without diamond
    here(dock),
    have(boat),
    have(diamond),
    money(M),
    NewM is M + 100,
    retract(money(M)),
    asserta(money(NewM)),
    retract(have(boat)),
    asserta(traded(fisherman)),
    write('You sold the boat for 100 coins.'), nl,
    write('Though, you may be missing something at the river...'), nl, !.
sell(diamond) :- % Trade diamond for bow at the canyon
    here(canyon),
    have(diamond),
    retract(have(diamond)),
    asserta(have(bow)),
    asserta(traded(archer)),
    write('The archer trades you a bow for the diamond.'), nl, !.
sell(diamond) :- % Attempt to trade diamond without having it
    here(canyon),
    \+ have(diamond),
    write('You don\'t have a diamond to trade.'), nl, !.
sell(Item) :- % Attempt to sell a valid item in the wrong location
    item(Item),
    \+ have(Item),
    write('You don\'t have a '), write(Item), write(' to sell.'), nl, !.
sell(Item) :- % Attempt to sell a valid item in the wrong location
    item(Item),
    write('You cannot sell the '), write(Item), write(' here.'), nl, !.
sell(Item) :- % Attempt to sell an invalid item
    \+ item(Item),
    write('You cannot sell a '), write(Item), write('. It is not a valid item.'), nl.
                                                            
% =======================
% Transportation System
% =======================

% Ride a boat from river to dock, if conditions are met
ride(boat) :-
    here(river),  % Player must be at river
    take(boat),
    location(boat, river),  % Boat must be at river
    retract(location(boat, river)),
    asserta(location(boat, dock)),  % Move boat to dock
    moveto(dock),  % Move player to dock
    write('You ride the boat to the dock.'), nl,
    look, !.
ride(boat) :-
    here(river),
    \+ location(boat, river),
    write('There is no boat here to ride.'), nl, !.
ride(boat) :-
    here(dock),
    write('The boat cannot be ridden from the dock.'), nl, !.
ride(boat) :-
    \+ here(river),
    write('You can only ride the boat at the river.'), nl, !.
ride(Transport) :-
    item(Transport),
    write('You cannot ride the '), write(Transport), write(' here.'), nl, !.
ride(Transport) :-
    \+ item(Transport),
    write('You cannot ride a '), write(Transport), write('. It is not a valid transport.'), nl.

% =======================
% Lighting System
% =======================

% Light the hay if player has hay and matches
light(hay) :-
    have(hay),
    have(matches),
    asserta(lit(hay)),
    write('You light the hay, illuminating the darkness.'), nl, !.
light(hay) :- % Attempt to light hay without matches
    have(hay),
    \+ have(matches),
    write('You need matches to light the hay.'), nl, !.
light(hay) :- % Attempt to light hay without having it
    \+ have(hay),
    write('You don\'t have any hay to light.'), nl, !.
light(matches) :- % Attempt to light matches (which can't be lit on their own)
    have(matches),
    write('You can\'t light the matches on their own. Try lighting something with them.'), nl, !.
light(Item) :- % Attempt to light a valid item that can't be lit
    item(Item),
    write('You cannot light the '), write(Item), write('.'), nl, !.
light(Item) :- % Attempt to light an invalid item
    \+ item(Item),
    write('You cannot light a '), write(Item), write('. It is not a valid item.'), nl.

% =======================
% Command Parsing System
% =======================

% Parse and execute player commands
get_command(C) :- 
  read_line_to_string(user_input, String),
  tokenize_atom(String, L),
  command(C,L), !.
get_command(_) :- 
  write('I don\'t understand, please try again.'), nl,
  fail.

% Define and normalize command structures
command(C,L) :- elim(NL,L), C =.. NL, !.
elim([talk,X], [talk,to,the,X,'.']) :- !.
elim([talk,X], [talk,to,X,'.']) :- !.
elim(NL, [V,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,to,the,N,'.']) :- NL = [V,N], !.
elim(NL, [V,'.']) :- NL = [V], !.

% Initialize game when loaded
:- init_game.
