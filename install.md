Follow the instructions for a2

Run by running make play in project directory


In a Mac Environment:
-Download X-Quartz (https://www.xquartz.org)
-Run 'opam update', 'opam upgrade' and 'opam install graphics' in the terminal.
-After downloading X-Quartz, restart your computer to make sure X-Quartz downloaded.
-Run 'make play' in the file directory from the X-Quartz terminal

In a Windows environment:
- Run 'sudo apt install libsdl2' and 'sudo apt install pkgconfig' in the terminal. 
- Run 'opam update', 'opam upgrade' and 'opam install graphics' in the terminal.
- Run 'make play' in the file directory to launch the game
(We want to note that we are all Mac users and are not 100% sure about windows environment)

To see an example of correct answer, for 'Name Something That Can Be Spoiled', you can input in 'milk' and 'kids'. 
At the moment, the random fucntion (''is_random_order'')is turned off. This function allows the questions from the JSON file to be generated randomly instead of going in error. The player can turn this function on by setting ''is_random_order'' to true.

