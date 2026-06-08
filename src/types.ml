type action =
  | LEFT
  | RIGHT

type transition = 
{
  read : char;
  to_state : string;
  write : char;
  action : action;
}

type machine = 
{
  name : string;
  alphabet : char list;
  blank : char;
  states : string list;
  initial : string;
  finals : string list;
  transitions : (string * transition list) list;
}

type tape =
{
   left : char list; 
   head : char; 
   right : char list;
}