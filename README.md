# PokemonChallenge
A solution to the Softwire Support Team Pokémon Challenge in Scala:

> There are [quite a lot of Pokémon](http://pokeapi.co/api/v1/pokedex/) now. Let the set of all Pokémon be the Pokédex.   
> We can take the power set of the Pokédex, giving us a set of all subsets of Pokémon, and then define a sort of order on the subsets:
> 
> S1 < S2 <=> The number of Pokémon in S1 is less than the number in S2, or, if they are equal, then the total number of characters in the names of S1 is less than those in S2
> 
> So:
> * {xatu}  < {smeargle}
> * {smeargle} < {xatu, ratatta}
> * {ratatta} == {geodude}
> * {charmeleon} < {muk, grimer}
> 
> I'm not sure what kind of ordering this represents, hopefully it makes sense though.
> 
> So, the challenge is – can you find a minimal set S of all subsets of the Pokédex, such that all the letters of the alphabet appear in S?

This currently gets the correct answer in ~10.5 seconds. I do have ideas to improve that - some left as comments in the code - but colleagues have got it [under a second](https://github.com/MatthewRichards/PokemonChallenge) by using bitmasks instead of strings and it might take a fair overhaul to beat that :-(
