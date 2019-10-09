
# Improving Elm project in Glitch 
_What seemed like a straightforward “itch to fetch”, ended up as a six hour marathon._

Suddenly, I had this itch to fetch the date information of the [**bit.ly**](https://bit.ly) shorteners that I was displaying in my Elm App. So, I went about my urge, fully knowing that it is a very straightforward ask, and this time, I would document my progress as I go along. 

## Fetching it from Glitch

I have been using Glitch to modify my Elm Apps for some time now, and I kind of like it. Although, it is not as intuitive as doing it on Cloud9 or on your local laptop, it has the advantage of being hosted and deployed immediately on the glitch platform itself. 

1. First,  I fire up my existing version of the App by typing [http://bitly-elm.glitch.me/](http://bitly-elm.glitch.me/) which brings up the app as shown below: 

	> ![img](https://i.imgur.com/eTskxgX.jpg)

2. Then I start reviewing my code on Glitch and immediately notice that I have this data type which is the basis of the entire app:  
```haskell
{-| Link is the basic data type that is used to implement the logic
for this program. Link is converted to HayString for some reason.
I used HayString during the testing phase, and will need to refactor it.
Alternatively, by leaving that in, we can convert some other type
and search on that as well. Something to ponder...
-}
type alias Link =
    { title : String
    , keyword_link : Maybe String
    , long_url : String
    , tags : List String
    }
``` 
and it needs date information to be captured, before I can even think of displaying it. Now, where to get this information? 

3. Then I bring up the test data which is available at [https://api.myjson.com/bins/skw8e](https://api.myjson.com/bins/skw8e) which immediately shows me two extra fields: 
	- Created date  - always has a valid timestamp value 
	- Modified date - by default it is 0 

	>![test](http://bit.ly/2OzWX2o)

4. Since I have done this so many times in the past, I know I have to go and modify my JSON decoders which is were these two pieces of data can be obtained. 

	> ![json](http://bit.ly/2AXxGHr) 

5. Now comes the Googling part for deciding what it takes to convert a timestamp to a date string which is worth displaying. 
	> ![time](http://bit.ly/decodeTime)

## Incremental approach

While I am tempted to get to the date string and displaying it, I decide, let me take some baby steps. It has been a while, so let me just display the timestamp as a string, before I go back and figure out how to display a Date information. 

So, I went ahead and edited in the two additional fields into the linkDecoder function : 

![decodeEdit](https://files.gitter.im/kgashok/advik/2mSx/Screenshot-of-main.elm---bitly-elm-2-.jpg)

## Fixing an error
Immediately, from the Log, I noticed that the Elm compiler (which runs automatically whenever a change is made)  

![error](https://files.gitter.im/kgashok/advik/Zk2F/Screenshot-2019-10-09-at-23.49.21.png)

## Second Error
Immediately, the compiler gives me another error, which is almost guiding me to fix things in a progressive manner? 

![typeError](https://files.gitter.im/kgashok/advik/r91n/Screenshot-of-main.elm---bitly-elm-3-.jpg)

This means, I have to go and edit things in the `Link` datatype, which now looks as follows: 

```haskell
type alias Link =
    { title : String
    , keyword_link : Maybe String
    , long_url : String
    , tags : List String
    , created_at : String
    , modified_at : String
    }
```
And viola! No compile errors in the Log Console. 

## Invoking the elm-format tool
Now, I launch the full Console log within Glitch so that I can use the `elm-format` to make sure my edits conform to the coding convention in Elm. I am a big stickler for properly formatted code. 

![format](https://files.gitter.im/kgashok/advik/l5dK/Screenshot-of-Glitch-Console.jpg)

And then I run the `optmize.sh` script, which actually now updates the files which are actually used to control the app: 
```
	public/elm.js  
	public/elm.min.js  
```

## Fixing the View Part 
Now, things are available to be displayed, the fun part begins. The view part of the Elm project now needs to be tinkered with. 

## Backup, backup
I now realized that I must have first started with raising an Issue for this in Github. Better late than never. 

![issue](https://files.gitter.im/kgashok/advik/YqMO/Screenshot-of-Add-timestamp-information-for-the-links-Issue-57-kgashok_elm-for-bitly.jpg)

So, there you go. Now that I have an issue, I can do a commit to **github** back with the modified code, so far. 

## Did I check the run-time? 

No, I did not - and so I am in for a surprise. The app does not show any links. But thankfully, because I am displaying an error in the display, I have this to go from: 

![run-time-error](https://files.gitter.im/kgashok/advik/CL19/Screenshot-2019-10-10-at-00.36.33.png)

The fix is rather elementary: the JSON data contains an integer, whereas my decoder is expecting a string. So, that has been taken care of as follows: 

```haskell
type alias Link =
    { title : String
    , keyword_link : Maybe String
    , long_url : String
    , tags : List String
    , created_at : Int
    , modified_at : Int
    }

```

## Meanwhile, the issues gets updated 

By using the issue number (`#57`) in the comments related to the interim exports from glitch to github, the issue on github gets updated, automatically. 

![issueUpdates](https://files.gitter.im/kgashok/advik/EWH3/Screenshot-2019-10-10-at-01.05.43.png)

## Detour done, back to View 

Now, I hone in on the right area where I need to plug in the `created_at` data that is now available in HayString's `created` field. 

Now, comes the decision, where do I hang this timestamp information? I went to the `bit.ly` site and saw it is separate and on top of the link. Probably the right place for now. I can always come and change it. 

![placement](https://files.gitter.im/kgashok/advik/Oyo1/Screenshot-2019-10-10-at-01.30.33.png)

## The Real Work 

`displayURL` is where the real action is. This is what decides what displayed and how. For now, I am happy with displaying the integer as an integer. And then come back do something fanciful. 
  > ![edits](https://files.gitter.im/kgashok/advik/3lgP/Screenshot-2019-10-10-at-01.42.29.png)

## Initial Success! 

  >  ![success1](https://files.gitter.im/kgashok/advik/K4bM/Screenshot-2019-10-10-at-01.38.50.png)

## Some CSS styling 

![style](https://files.gitter.im/kgashok/advik/yHzf/Screenshot-2019-10-10-at-02.03.39.png)

## Let's get some Date Information

First step, is to convert the integer into milliseconds. And then use a formatter to get data information. 

## Breakthrough at 3.30am

Thanks to the Iso8601 elm-package and the [SO answer](https://stackoverflow.com/a/36203937/307454), my night was saved. 

![unix](https://files.gitter.im/kgashok/advik/bBDA/Screenshot-of-Glitch-Console-1-.jpg)

and it resulted in this rather non-trivial [fix](https://j.mp/bugFixForTime) which finally resolved the frustrating problem. 

## Need to refactor
My `displayURL` function has gotten hairy because of the need to convert and display Posix values to Human readable tokens. That is not acceptable. It needs to be refactored. But that can be done later. 

## The final capture

_At 4.08am_ 
![final](https://files.gitter.im/kgashok/advik/DR3z/Screenshot-of-Elm-for-Bitly.jpg)

## Closing the issue on Github

And this is the most satisfying part of documenting an incremental improvement to a side project. To use `Resolved #XX` (`XX` being the issue number), and in this case,  **`Resolved #57`** was a big high. 

![resolved](https://files.gitter.im/kgashok/advik/0r3k/Screenshot-of-Add-timestamp-information-for-the-links-Issue-57-kgashok_elm-for-bitly-1-.jpg)


