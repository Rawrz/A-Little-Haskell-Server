Roseline Okpara

This is a really basic server.

I read a paper that explained the core concepts on how to implement a high performance server. However, I really just wanted to learn some haskell so I created something very simple.

Set up

The paper described how they used a lot of external libraries to implement 
their server. One of which being an HTML library called Lucid. It's all implemented.

within the scope of GHC so you can install it with:

>> cabal install lucid

Running the main function will start the server on localhost:9000
This will show the requests made and a little welcome message. The requests and 
responses can also be viewd on the commard line.

I put in a test file and a test directory to test paths and file searches.

