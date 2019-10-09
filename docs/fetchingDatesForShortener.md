<!DOCTYPE html>
<html>

<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>fetchingDatesForShortener</title>
  <link rel="stylesheet" href="https://stackedit.io/style.css" />
</head>

<body class="stackedit">
  <div class="stackedit__left">
    <div class="stackedit__toc">
      
<ul>
<li><a href="#improving-elm-project-in-glitch">Improving Elm project in Glitch</a>
<ul>
<li><a href="#fetching-it-from-glitch">Fetching it from Glitch</a></li>
<li><a href="#incremental-approach">Incremental approach</a></li>
<li><a href="#fixing-an-error">Fixing an error</a></li>
<li><a href="#second-error">Second Error</a></li>
<li><a href="#invoking-the-elm-format-tool">Invoking the elm-format tool</a></li>
<li><a href="#fixing-the-view-part">Fixing the View Part</a></li>
<li><a href="#backup-backup">Backup, backup</a></li>
<li><a href="#did-i-check-the-run-time">Did I check the run-time?</a></li>
<li><a href="#meanwhile-the-issues-gets-updated">Meanwhile, the issues gets updated</a></li>
<li><a href="#detour-done-back-to-view">Detour done, back to View</a></li>
<li><a href="#the-real-work">The Real Work</a></li>
<li><a href="#initial-success">Initial Success!</a></li>
<li><a href="#some-css-styling">Some CSS styling</a></li>
<li><a href="#lets-get-some-date-information">Let’s get some Date Information</a></li>
<li><a href="#breakthrough-at-3.30am">Breakthrough at 3.30am</a></li>
<li><a href="#need-to-refactor">Need to refactor</a></li>
<li><a href="#the-final-capture">The final capture</a></li>
<li><a href="#closing-the-issue-on-github">Closing the issue on Github</a></li>
</ul>
</li>
</ul>

    </div>
  </div>
  <div class="stackedit__right">
    <div class="stackedit__html">
      <h1 id="improving-elm-project-in-glitch">Improving Elm project in Glitch</h1>
<p><em>What seemed like a straightforward “itch to fetch”, ended up as a six hour marathon.</em></p>
<p>Suddenly, I had this itch to fetch the date information of the <a href="https://bit.ly"><strong>bit.ly</strong></a> shorteners that I was displaying in my Elm App. So, I went about my urge, fully knowing that it is a very straightforward ask, and this time, I would document my progress as I go along.</p>
<h2 id="fetching-it-from-glitch">Fetching it from Glitch</h2>
<p>I have been using Glitch to modify my Elm Apps for some time now, and I kind of like it. Although, it is not as intuitive as doing it on Cloud9 or on your local laptop, it has the advantage of being hosted and deployed immediately on the glitch platform itself.</p>
<ol>
<li>
<p>First,  I fire up my existing version of the App by typing <a href="http://bitly-elm.glitch.me/">http://bitly-elm.glitch.me/</a> which brings up the app as shown below:</p>
<blockquote>
<p><img src="https://i.imgur.com/eTskxgX.jpg" alt="img"></p>
</blockquote>
</li>
<li>
<p>Then I start reviewing my code on Glitch and immediately notice that I have this data type which is the basis of the entire app:</p>
</li>
</ol>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token comment">{-| Link is the basic data type that is used to implement the logic
for this program. Link is converted to HayString for some reason.
I used HayString during the testing phase, and will need to refactor it.
Alternatively, by leaving that in, we can convert some other type
and search on that as well. Something to ponder...
-}</span>
<span class="token keyword">type</span> <span class="token hvariable">alias</span> <span class="token constant">Link</span> <span class="token operator">=</span>
    <span class="token punctuation">{</span> <span class="token hvariable">title</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">keyword_link</span> <span class="token operator">:</span> <span class="token constant">Maybe</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">long_url</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">tags</span> <span class="token operator">:</span> <span class="token constant">List</span> <span class="token constant">String</span>
    <span class="token punctuation">}</span>
</code></pre>
<p>and it needs date information to be captured, before I can even think of displaying it. Now, where to get this information?</p>
<ol start="3">
<li>
<p>Then I bring up the test data which is available at <a href="https://api.myjson.com/bins/skw8e">https://api.myjson.com/bins/skw8e</a> which immediately shows me two extra fields:</p>
<ul>
<li>Created date  - always has a valid timestamp value</li>
<li>Modified date - by default it is 0</li>
</ul>
<blockquote>
<p><img src="http://bit.ly/2OzWX2o" alt="test"></p>
</blockquote>
</li>
<li>
<p>Since I have done this so many times in the past, I know I have to go and modify my JSON decoders which is were these two pieces of data can be obtained.</p>
<blockquote>
<p><img src="http://bit.ly/2AXxGHr" alt="json"></p>
</blockquote>
</li>
<li>
<p>Now comes the Googling part for deciding what it takes to convert a timestamp to a date string which is worth displaying.</p>
<blockquote>
<p><img src="http://bit.ly/decodeTime" alt="time"></p>
</blockquote>
</li>
</ol>
<h2 id="incremental-approach">Incremental approach</h2>
<p>While I am tempted to get to the date string and displaying it, I decide, let me take some baby steps. It has been a while, so let me just display the timestamp as a string, before I go back and figure out how to display a Date information.</p>
<p>So, I went ahead and edited in the two additional fields into the linkDecoder function :</p>
<p><img src="https://files.gitter.im/kgashok/advik/2mSx/Screenshot-of-main.elm---bitly-elm-2-.jpg" alt="decodeEdit"></p>
<h2 id="fixing-an-error">Fixing an error</h2>
<p>Immediately, from the Log, I noticed that the Elm compiler (which runs automatically whenever a change is made)</p>
<p><img src="https://files.gitter.im/kgashok/advik/Zk2F/Screenshot-2019-10-09-at-23.49.21.png" alt="error"></p>
<h2 id="second-error">Second Error</h2>
<p>Immediately, the compiler gives me another error, which is almost guiding me to fix things in a progressive manner?</p>
<p><img src="https://files.gitter.im/kgashok/advik/r91n/Screenshot-of-main.elm---bitly-elm-3-.jpg" alt="typeError"></p>
<p>This means, I have to go and edit things in the <code>Link</code> datatype, which now looks as follows:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">type</span> <span class="token hvariable">alias</span> <span class="token constant">Link</span> <span class="token operator">=</span>
    <span class="token punctuation">{</span> <span class="token hvariable">title</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">keyword_link</span> <span class="token operator">:</span> <span class="token constant">Maybe</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">long_url</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">tags</span> <span class="token operator">:</span> <span class="token constant">List</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">created_at</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">modified_at</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">}</span>
</code></pre>
<p>And viola! No compile errors in the Log Console.</p>
<h2 id="invoking-the-elm-format-tool">Invoking the elm-format tool</h2>
<p>Now, I launch the full Console log within Glitch so that I can use the <code>elm-format</code> to make sure my edits conform to the coding convention in Elm. I am a big stickler for properly formatted code.</p>
<p><img src="https://files.gitter.im/kgashok/advik/l5dK/Screenshot-of-Glitch-Console.jpg" alt="format"></p>
<p>And then I run the <code>optmize.sh</code> script, which actually now updates the files which are actually used to control the app:</p>
<pre><code>	public/elm.js  
	public/elm.min.js  
</code></pre>
<h2 id="fixing-the-view-part">Fixing the View Part</h2>
<p>Now, things are available to be displayed, the fun part begins. The view part of the Elm project now needs to be tinkered with.</p>
<h2 id="backup-backup">Backup, backup</h2>
<p>I now realized that I must have first started with raising an Issue for this in Github. Better late than never.</p>
<p><img src="https://files.gitter.im/kgashok/advik/YqMO/Screenshot-of-Add-timestamp-information-for-the-links-Issue-57-kgashok_elm-for-bitly.jpg" alt="issue"></p>
<p>So, there you go. Now that I have an issue, I can do a commit to <strong>github</strong> back with the modified code, so far.</p>
<h2 id="did-i-check-the-run-time">Did I check the run-time?</h2>
<p>No, I did not - and so I am in for a surprise. The app does not show any links. But thankfully, because I am displaying an error in the display, I have this to go from:</p>
<p><img src="https://files.gitter.im/kgashok/advik/CL19/Screenshot-2019-10-10-at-00.36.33.png" alt="run-time-error"></p>
<p>The fix is rather elementary: the JSON data contains an integer, whereas my decoder is expecting a string. So, that has been taken care of as follows:</p>
<pre class=" language-haskell"><code class="prism  language-haskell"><span class="token keyword">type</span> <span class="token hvariable">alias</span> <span class="token constant">Link</span> <span class="token operator">=</span>
    <span class="token punctuation">{</span> <span class="token hvariable">title</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">keyword_link</span> <span class="token operator">:</span> <span class="token constant">Maybe</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">long_url</span> <span class="token operator">:</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">tags</span> <span class="token operator">:</span> <span class="token constant">List</span> <span class="token constant">String</span>
    <span class="token punctuation">,</span> <span class="token hvariable">created_at</span> <span class="token operator">:</span> <span class="token constant">Int</span>
    <span class="token punctuation">,</span> <span class="token hvariable">modified_at</span> <span class="token operator">:</span> <span class="token constant">Int</span>
    <span class="token punctuation">}</span>

</code></pre>
<h2 id="meanwhile-the-issues-gets-updated">Meanwhile, the issues gets updated</h2>
<p>By using the issue number (<code>#57</code>) in the comments related to the interim exports from glitch to github, the issue on github gets updated, automatically.</p>
<p><img src="https://files.gitter.im/kgashok/advik/EWH3/Screenshot-2019-10-10-at-01.05.43.png" alt="issueUpdates"></p>
<h2 id="detour-done-back-to-view">Detour done, back to View</h2>
<p>Now, I hone in on the right area where I need to plug in the <code>created_at</code> data that is now available in HayString’s <code>created</code> field.</p>
<p>Now, comes the decision, where do I hang this timestamp information? I went to the <code>bit.ly</code> site and saw it is separate and on top of the link. Probably the right place for now. I can always come and change it.</p>
<p><img src="https://files.gitter.im/kgashok/advik/Oyo1/Screenshot-2019-10-10-at-01.30.33.png" alt="placement"></p>
<h2 id="the-real-work">The Real Work</h2>
<p><code>displayURL</code> is where the real action is. This is what decides what displayed and how. For now, I am happy with displaying the integer as an integer. And then come back do something fanciful.</p>
<blockquote>
<p><img src="https://files.gitter.im/kgashok/advik/3lgP/Screenshot-2019-10-10-at-01.42.29.png" alt="edits"></p>
</blockquote>
<h2 id="initial-success">Initial Success!</h2>
<blockquote>
<p><img src="https://files.gitter.im/kgashok/advik/K4bM/Screenshot-2019-10-10-at-01.38.50.png" alt="success1"></p>
</blockquote>
<h2 id="some-css-styling">Some CSS styling</h2>
<p><img src="https://files.gitter.im/kgashok/advik/yHzf/Screenshot-2019-10-10-at-02.03.39.png" alt="style"></p>
<h2 id="lets-get-some-date-information">Let’s get some Date Information</h2>
<p>First step, is to convert the integer into milliseconds. And then use a formatter to get data information.</p>
<h2 id="breakthrough-at-3.30am">Breakthrough at 3.30am</h2>
<p>Thanks to the Iso8601 elm-package and the <a href="https://stackoverflow.com/a/36203937/307454">SO answer</a>, my night was saved.</p>
<p><img src="https://files.gitter.im/kgashok/advik/bBDA/Screenshot-of-Glitch-Console-1-.jpg" alt="unix"></p>
<p>and it resulted in this <a href="https://github.com/kgashok/elm-for-bitly/commit/f3096954f78d315114e6f65ef76fa32a36853220#r35434532">edit</a> which finally resolved the bug fix.</p>
<h2 id="need-to-refactor">Need to refactor</h2>
<p>My <code>displayURL</code> function has gotten hairy because of the need to convert and display Posix values to Human readable tokens. That is not acceptable. It needs to be refactored. But that can be done later.</p>
<h2 id="the-final-capture">The final capture</h2>
<p><em>At 4.08am</em><br>
<img src="https://files.gitter.im/kgashok/advik/DR3z/Screenshot-of-Elm-for-Bitly.jpg" alt="final"></p>
<h2 id="closing-the-issue-on-github">Closing the issue on Github</h2>
<p>And this is the most satisfying part of documenting an incremental improvement to a side project. To use <code>Resolved #XX</code> (<code>XX</code> being the issue number), and in this case,  <strong><code>Resolved #57</code></strong> was a big high.</p>
<p><img src="https://files.gitter.im/kgashok/advik/0r3k/Screenshot-of-Add-timestamp-information-for-the-links-Issue-57-kgashok_elm-for-bitly-1-.jpg" alt="resolved"></p>

    </div>
  </div>
</body>

</html>