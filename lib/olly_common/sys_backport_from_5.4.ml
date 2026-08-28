(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Sys

let signal_to_string s =
  if s = sigabrt then "SIGABRT"
  else if s = sigalrm then "SIGALRM"
  else if s = sigfpe then "SIGFPE"
  else if s = sighup then "SIGHUP"
  else if s = sigill then "SIGILL"
  else if s = sigint then "SIGINT"
  else if s = sigkill then "SIGKILL"
  else if s = sigpipe then "SIGPIPE"
  else if s = sigquit then "SIGQUIT"
  else if s = sigsegv then "SIGSEGV"
  else if s = sigterm then "SIGTERM"
  else if s = sigusr1 then "SIGUSR1"
  else if s = sigusr2 then "SIGUSR2"
  else if s = sigchld then "SIGCHLD"
  else if s = sigcont then "SIGCONT"
  else if s = sigstop then "SIGSTOP"
  else if s = sigtstp then "SIGTSTP"
  else if s = sigttin then "SIGTTIN"
  else if s = sigttou then "SIGTTOU"
  else if s = sigvtalrm then "SIGVTALRM"
  else if s = sigprof then "SIGPROF"
  else if s = sigbus then "SIGBUS"
  else if s = sigpoll then "SIGPOLL"
  else if s = sigsys then "SIGSYS"
  else if s = sigtrap then "SIGTRAP"
  else if s = sigurg then "SIGURG"
  else if s = sigxcpu then "SIGXCPU"
  else if s = sigxfsz then "SIGXFSZ"
  else "SIG(" ^ string_of_int s ^ ")"
