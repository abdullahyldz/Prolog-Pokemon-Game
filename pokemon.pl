:- include('pokemon_data.pl').

%This predicate first finds each trainer's Pokemons and their levels as lists (Pokemon_trainer)
%Then sends these lists to tournamenter predicate,
%WinnerTrainerList is calculated there
pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-
    pokemon_trainer(PokemonTrainer1,List1,Levels1),
	pokemon_trainer(PokemonTrainer2,List2,Levels2),
	tournamenter(List1,Levels1,List2,Levels2,PokemonTrainer1,PokemonTrainer2,WinnerTrainerList).

%This predicate takes 4 lists,2 PokemonList of Trainers and 2 PokemonLevelList of Trainers
%Splits the lists as head and the rest
%Searches for evolution for every head pokemon
%Then sends the Evolved version of them to fight,
%winner of fight will be the head of WinnerTrainerList (W_H)
%Searches for other pokemons to make them fight,until a list is empty
tournamenter([List1_head | List1_rest],[Level1_head | Level1_rest],[List2_head | List2_rest],[Level2_head|Level2_rest],T1,T2,[W_H|W_T]):-
	find_pokemon_evolution(Level1_head,List1_head,Evolved1),find_pokemon_evolution(Level2_head,List2_head,Evolved2),
	pokemon_fight(Evolved1, Level1_head, Evolved2, Level2_head,Pokemon1Hp, Pokemon2Hp, _),
	(Pokemon1Hp>=Pokemon2Hp -> W_H = T1 ; W_H = T2),
	tournamenter(List1_rest,Level1_rest,List2_rest,Level2_rest,T1,T2,W_T)
	.
%When list is empty,this is true
tournamenter([],[],[],[],_,_,[]).
%Searches for evolution,first calls pokemon_evolution predicate to see if it has evolution
%if found then searches for another evolution for that evolved pokemon
%if there is no evolution possible,then evolved version will be itself (Evolved = Pokemon)
%Cut prevents the middle results
find_pokemon_evolution(PokemonLevel,Pokemon,EvolvedPokemon):-
	(pokemon_evolution(Pokemon,X,LVL),
	LVL=<PokemonLevel,
	find_pokemon_evolution(PokemonLevel,X,EvolvedPokemon)),!;
	EvolvedPokemon = Pokemon.
%Calculates the Heaşth,Attack,Defence points of given pokemon
%Calls pokemon_stats/5 to get the Pokemon's base points
%Then applies an operation to achieve given level's values 
pokemon_level_stats(PokemonLevel, Pokemon, HEALTH, ATTACK, DEFENCE):-pokemon_stats(Pokemon,_,HealthPoint,Attack,Defence),
HEALTH is 2*PokemonLevel+HealthPoint,
ATTACK is (PokemonLevel+Attack),
DEFENCE is (PokemonLevel+Defence) .

%This predicate can find all Attacker,Defender,Multiplier values when given at least one of them.
%POKEMON_TYPES represents all pokemon types 
%type_chart_attack defines a type, and for each type, attack multipliers against all types are
%listed. The ordering of the TypeMultipliers are the same with pokemon types.
%This predicate calls single_type\3
single_type_multiplier(Attacker,Defender,Multiplier):-

      pokemon_types(POKEMON_TYPES), type_chart_attack(Attacker, TypeMultipliers),
      single_type(Defender, POKEMON_TYPES, Multiplier, TypeMultipliers).
%This predicate searches POKEMON_TYPES and TypeMultipliers,at the same time. Calls itself with the rest of the list
single_type(Defender,[_|List1],Multiplier,[_|List2]):-
	single_type(Defender,List1,Multiplier,List2).
%This recursion finds Defender or Multiplier,when given one of Attacker,Defender,Multiplier
single_type(Defender,[Defender|_],Multiplier,[Multiplier|_]).

%type_multiplier\3 calls typer\3
type_multiplier(AttackerType, DefenderTypeList, Multiplier):-
    typer(AttackerType,DefenderTypeList,Multiplier).

%typer\3 splits DefenderTypeList as HD and List
%Sends every HD and AttackerType and Multipliers to single_type_multiplier
%Also calls itself with the rest of the DefenderTypeList (List) 
%Multiplier will be the multiplication of results coming from recursion and the single_type_multiplier
typer(AttackerType,[HD|List],Multiplier):-
	  single_type_multiplier(AttackerType,HD,Multipliers),typer(AttackerType,List,MULT),Multiplier is MULT*Multipliers.
%Base case for typer, Multiplier is 1
typer(_,[],1).

%This predicate finds better Multiplier that AttackerPokemon can apply to DefenderPokemon
%First finds every Pokemon's Types(pokemon_stats)
%then calls pokemon_typer\3 with those types
pokemon_type_multiplier(AttackerPokemon, DefenderPokemon, Multiplier):-

	pokemon_stats(AttackerPokemon,TypeList1,_,_,_),
	pokemon_stats(DefenderPokemon,TypeList2,_,_,_),
	pokemon_typer(TypeList1,TypeList2,Multiplier).

%pokemon_typer calls type_multiplier for every Type of AttackerPokemon
%also calls itself with the rest of the types that AttackerPokemon has
%Bigger of the multiplier coming from recursion and type_multiplier is passed to M
pokemon_typer([Attack_Head|Attack_List],DefenderList,M):-
	type_multiplier(Attack_Head,DefenderList,MULT),
	pokemon_typer(Attack_List,DefenderList,MULT2),
	(   MULT >= MULT2 -> M is MULT;M is MULT2).

%Base case for pokemon_typer
%Multiplier is made zero, so it wont be selected since type_multiplier's multiplier will be greater than zero
pokemon_typer([],_,0).

%pokemon_attack\5 first calls pokemon_level_stats to get Attacker's AttackDamage and Defender's Defence
%then calls pokemon_type_multiplier to get the multiplier that Attacker has against Defender
%finally,damage is easily written with formula 0.5*Level*(Attack/Defence)*Multiplier +1
pokemon_attack(Attacker,AttackerLevel,Defender,DefenderLevel,Damage):-

	pokemon_level_stats(DefenderLevel,Defender,_,_,DEFENCE_D),
	pokemon_level_stats(AttackerLevel,Attacker,_,ATTACK_A,_),
	pokemon_type_multiplier(Attacker,Defender,Multiplier),
	Damage is ((0.5*AttackerLevel*(ATTACK_A/DEFENCE_D)*Multiplier)+1).

%pokemon_fight\7 first calls pokemon_level_stats with Pokemon and PokemonLevels to get initial health values
%They fight each other, Damages are known 
%Then it calls pokemon_fighter with initial Hp's, Damages and Rounds
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level,
Pokemon1Hp, Pokemon2Hp, Rounds):-
  pokemon_level_stats(Pokemon1Level,Pokemon1,HEALTH1,_,_),
  pokemon_level_stats(Pokemon2Level,Pokemon2,HEALTH2,_,_),
  pokemon_attack(Pokemon1,Pokemon1Level,Pokemon2,Pokemon2Level,Damage1),
  pokemon_attack(Pokemon2,Pokemon2Level,Pokemon1,Pokemon1Level,Damage2),
  pokemon_fighter(Damage1,HEALTH1,Pokemon1Hp,Damage2,HEALTH2,Pokemon2Hp,Rounds).

%Every call of this function increases Round by 1
%Every call their health reduces by opponent's damage 
%They are left with Remain1 and Remain2
%If their remaining healths are greater than zero, they fight again (recursive call)
%if one of the health is less than zero then Rounds is returned as 1 and
%HP1_END/HP2_END results are returned as Remain1 and Remain2
pokemon_fighter(Damage1,HP1_BEGIN,HP1_END,Damage2,HP2_BEGIN,HP2_END,Rounds):-
Remain1 is HP1_BEGIN-Damage2,
Remain2 is HP2_BEGIN-Damage1,
((Remain1 >= 0,Remain2 >=0) ->  pokemon_fighter(Damage1,Remain1,HP1_END,Damage2,Remain2,HP2_END,ROUND),Rounds is ROUND+1 ,!);
Remain1 is HP1_BEGIN-Damage2,
Remain2 is HP2_BEGIN-Damage1,
Rounds is 1,
HP1_END is Remain1,
HP2_END is Remain2.

%best_pokemon\4  first finds the all pokemon list by calling findall(..) as POKE_LIST
%POKE_LIST is sent to best_finder\5 along with EnemyPokemon,LevelCap,RemainingHP
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon):-
         findall(POKE,pokemon_stats(POKE, _, _, _, _),POKE_LIST),
         best_finder(EnemyPokemon,LevelCap,POKE_LIST,RemainingHP,BestPokemon).

%best_finder\5 predicate
%POKE_LIST is split as Head and Rest
%Every head fights with EnemyPokemon, Pokemon1Hp represents the remaining HP of attacker pokemon
%then recursion continues,every pokemon fight with EnemyPokemon
%Every recursion holds RecursionBestPoke that fights well with EnemyPokemon so far and its RecursionBestHP so far.
%Recursion result and Pokemon1Hp in pokemon_fight are compared then greater HP is passed as BestPokemon,BestHP
best_finder(EnemyPokemon,LevelCap,[POKE_HEAD|POKE_REST],BestHP,BestPokemon):-
    pokemon_fight(POKE_HEAD, LevelCap, EnemyPokemon, LevelCap, Pokemon1Hp, _, _),
    best_finder(EnemyPokemon,LevelCap,POKE_REST,RecursionBestHP,RecursionBestPoke),
    (RecursionBestHP>=Pokemon1Hp -> (BestHP = RecursionBestHP,BestPokemon = RecursionBestPoke) ; (BestHP = Pokemon1Hp,BestPokemon = POKE_HEAD)).

%best_finder\5  base case ,returns -900 as BestHp , so it won't be selected better when returned.
best_finder(_,_,[],-900,_):-!.

%best_pokemon_team\2 predicate first calls pokemon_trainer to learn OpponentTrainer's list of pokemon
%Then send the info to best_team_finder
best_pokemon_team(OpponentTrainer, PokemonTeam):-
    pokemon_trainer(OpponentTrainer, OpponentList, OpponentLevel),
    best_team_finder(OpponentList,OpponentLevel,PokemonTeam).

%best_team_finder\3 splits the pokemons as Opponent_H and Opponent_R and
%their levels as Level_H and Level_R
%finds best pokemon against Opponent_H as Best_H, which is first element of PokemonTeam
%calls itself with the Opponent_R,Level_R to find BEST_R recursively
best_team_finder([Opponent_H|Opponent_R],[Level_H|Level_R],[BEST_H|BEST_R]):-
    best_pokemon(Opponent_H,Level_H,_,BEST_H),
    best_team_finder(Opponent_R,Level_R,BEST_R).

%When OpponentList is empty then no best pokemon is calculated
best_team_finder([],[],[]).

%pokemon_types\3 uses setof to avoid repetitions, it searches for every pokemon and sends them to pokemon_types_2
%every member pokemon of InitialPokemonList is added to list if at least pne of their type is in the TypeList
%pokemon_types_2 controls this condition
pokemon_types(TypeList, InitialPokemonList, PokemonList) :-
    setof(Pokemon, (member(Pokemon, InitialPokemonList), pokemon_types_2(TypeList, Pokemon)) , PokemonList).

%Splits the TypeList as H and TypeList
%if H is a member of Pokemon's PokemonTypeList then return true (added to PokemonList)
%else search for other types in TypeList for coincidences
pokemon_types_2([H|TypeList], Pokemon) :-
    pokemon_stats(Pokemon, PokemonTypeList, _, _, _),
    (member(H, PokemonTypeList); pokemon_types_2(TypeList, Pokemon)).

%This complicated predicate is simplified using different predicates.
%Refer to those predicates for explanation
%All Pokemon names are grouped under POKES
%PokemonTeamL has pokemons that likes LikedTypes
%PokemonTeamD has pokemon that dislikes DisLikedTypes
%ord_subtract is a built-in function to find the difference of two lists
%quick_sort2 sorts PokemonDiff according to Criterion then constructs PokemonTeamSorted
%counterL takes Count number of elements from PokemonTeamSorted then puts them under PokemonCounted
%generator takes a list of Pokemons then returns a list of elements that shows stats of pokemons
generate_pokemon_team(LikedTypes, DislikedTypes, Criterion, Count,PokemonTeam):-
    findall(Pok,pokemon_stats(Pok, _, _, _, _),POKES),
    pokemon_types(LikedTypes,POKES,PokemonTeamL),
    pokemon_types(DislikedTypes,POKES,PokemonTeamD),
    ord_subtract(PokemonTeamL,PokemonTeamD,PokemonTeamDiff),
    quick_sort2(PokemonTeamDiff,PokemonTeamSorted,Criterion),
    counterL(PokemonTeamSorted,PokemonCounted,Count),
    generator(PokemonCounted,PokemonTeam).

%counterL takes 3 parameters,iterates over elements Count number of times
%When Count is 0, it stops
counterL(_,[],0).
counterL([H|PokemonTeamSorted],[H2|PokemonCounted],Count):-
    H2 = H,COUNT is (Count-1),
    counterL(PokemonTeamSorted,PokemonCounted,COUNT).

%Takes a list of Pokemons, creates a list then fills it with Pokemon stats
generator([],[]).
generator([P|PokemonTeamDiff],[H2|PokemonTeamNotSorted]):-
    pokemon_stats(P, _, H, A, D),
    Lis = [P,H,A,D],
    H2 = Lis,
    generator(PokemonTeamDiff,PokemonTeamNotSorted).
%taken from http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
%then corrected according to the problem
quick_sort2(List,Sorted,Criterion):-q_sort(List,[],Sorted,Criterion).
q_sort([],Acc,Acc,_).
q_sort([H|T],Acc,Sorted,Criterion):-
    pivoting(H,T,L1,L2,Criterion),
    q_sort(L1,Acc,Sorted1,Criterion),q_sort(L2,[H|Sorted1],Sorted,Criterion) .

pivoting(_,[],[],[],_).
pivoting(H,[X|T],[X|L],G,Criterion):-
(
%According to Criterion given,it chooses A1 and A2, as Health/Attack/Defence values of two pokemons
%sorts the list
(Criterion = h-> (pokemon_stats(X, _, A1, _, _),pokemon_stats(H, _, A2, _, _)));
(Criterion = a-> (pokemon_stats(X, _, _, A1, _),pokemon_stats(H, _, _, A2, _)));
(Criterion = d-> (pokemon_stats(X, _, _, _, A1),pokemon_stats(H, _, _, _,A2)))
),
%recursive pivoting,always sends Criterion
A1=<A2,pivoting(H,T,L,G,Criterion   ).

%According to Criterion given,it chooses A1 and A2, as Health/Attack/Defence values of two pokemons
%sorts the list
pivoting(H,[X|T],L,[X|G],Criterion):-
(
(Criterion = h-> (pokemon_stats(X, _, A1, _, _),pokemon_stats(H, _, A2, _, _)));
(Criterion = a-> (pokemon_stats(X, _, _, A1, _),pokemon_stats(H, _, _, A2, _)));
(Criterion = d-> (pokemon_stats(X, _, _, _, A1),pokemon_stats(H, _, _, _,A2)))
),
A1>A2,pivoting(H,T,L,G,Criterion).


