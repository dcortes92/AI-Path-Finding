% Instituto Tecnológico de Costa Rica
% Escuela de Ingeniería en Computación
% Inteligencia Artificial
% I Tarea Programada 
% Daniel Cortés Sáenz, 201120152
% Descripción: implementación de algorítmos de búsqueda de ruta óptima informados:
%				1. Greedy:
%				2. A*:
%				3. Jumping positions.

-module(search).

-author('dcortes92@hotmail.com').
-export([main/0]).

main() -> board ! {get_pos, self()}, receive X -> X end.