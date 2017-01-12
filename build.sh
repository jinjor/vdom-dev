elm-make --debug src/Counter.elm --output=docs/counter.html &&
elm-make --debug src/Clock.elm --output=docs/clock.html &&
elm-make --debug src/DragAndDrop.elm --output=docs/drag-and-drop.js &&
elm-make --debug src/LargeModel.elm --output=docs/large-model.html &&
elm-make --debug src/MsgTree.elm --output=docs/msg-tree.html &&
elm-make --debug src/Animation.elm --output=docs/animation.html &&

echo "<h1>Examples</h1><ul>" > docs/index.html
ls docs | grep .html | grep -v index.html | awk '{ print "<li><a href="$1">"$1"</a></li>" }' >> docs/index.html
echo "</ul>" >> docs/index.html
echo "done"
