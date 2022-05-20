(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1: string, s2: string) =
    s1 = s2;

(* solutions for problem 1 here *)

fun all_except_option(str,list)=
   case list of
      [] => NONE
      | x::list' => 
         if same_string(x,str) 
         then SOME(list')                  
         else 
            case all_except_option(str,list') of
               NONE=>NONE
               | SOME list'=>SOME(x::list')


fun get_substitutions1(lst: string list list, s: string) : string list =
   case lst of
      [] => []
      | x::xs =>
         case all_except_option(s, x) of
         NONE => get_substitutions1(xs, s)
         | SOME(names) => names @ get_substitutions1(xs, s);


fun get_substitutions2(lst: string list list, s: string) : string list =
  let
    fun accumulate(lists, acc) =
      case lists of
        [] => acc
        | x::xs =>
          case all_except_option(s, x) of
            NONE => accumulate(xs, acc)
            | SOME(names) => accumulate(xs, acc @ names);
  in
    accumulate(lst, [])
  end;


fun similar_names (substitutions: string list list, 
                   {first=first_name: string, middle=middle_name: string, last=last_name: string}) 
                   : {first: string, middle: string, last: string} list =
   let
      fun generate_names(substitutions: string list, 
                         names: {first: string, middle: string, last: string} list) 
                         : {first: string, middle: string, last: string} list = 
         case substitutions of
            [] => names
            | x::xs =>
               generate_names(xs, names @ [{first=first_name, middle=middle_name, last=last_name}]);
   in
      generate_names (
         get_substitutions2(substitutions, first_name),
         [{first=first_name, middle=middle_name, last=last_name}]
      )
   end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* solutions for problem 2 here *)

fun card_color(card, _) =
   case card of
      Diamonds => Red
      | Hearts => Red
      | others => Black;


fun card_value(_, card) =
   case card of
      Num int => int
      | Ace => 11
      | others => 10;


fun remove_card(cs, c, e) =
   case cs of
      [] => raise e
      | x::xs =>
            if x = c
            then xs
            else x::remove_card(xs, c, e);
   

fun all_same_color(cs: card list) =
   case cs of
      [] => true
      | x::[] => true
      | x::y::xs =>
         card_color(x) = card_color(y)
         andalso all_same_color(y::xs);


fun sum_cards(cs: card list) =
   let
      fun accumulate(cs, acc) = 
         case cs of
         [] => acc
         | x::xs => accumulate(xs, acc + card_value(x));
   in
      accumulate(cs, 0)
   end;


fun score(cs: card list, goal: int) =
   let
      val total = sum_cards(cs);
      val prevScore = if total > goal then 3 * (total - goal) else (goal - total);
   in
      if all_same_color(cs)
      then prevScore div 2
      else prevScore
   end;


fun officiate(cards_list: card list, moves_list: move list, goal_score: int) : int =
   let
      fun next_move(cards_list, player_cards, moves_list) = 
         case moves_list of
         [] => score(player_cards, goal_score) | to_do::xs =>
            let
               fun get_card(cards_list, player_cards, moves_list) = 
                  case cards_list of
                     [] => score(player_cards, goal_score) | to_do::xs =>
                           if sum_cards(to_do::player_cards) > goal_score
                           then score(to_do::player_cards, goal_score)
                           else next_move(xs, to_do::player_cards, moves_list)

               fun discard_card(cards_list, player_cards, card, moves_list) =
                  next_move(cards_list, remove_card(player_cards, card, IllegalMove), moves_list);
            in
               case to_do of
                  Draw => get_card(cards_list, player_cards, xs) | Discard card => discard_card(cards_list, player_cards, card, xs)
            end;
   in
      next_move(cards_list, [], moves_list)
   end;