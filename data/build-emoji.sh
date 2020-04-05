#!/usr/bin/env nix-shell
#!nix-shell -i sh -p jq

curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Activity.json | jq '.activities.activity[] | "\(.key) = \(.value)"' > emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Flags.json | jq '.flags.flag[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Food.json | jq '.foods.food[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Nature.json | jq '.natures.nature[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Objects.json | jq '.objects.object[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/People.json | jq '.peoples.people[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Symbols.json | jq '.symbols.symbol[] | "\(.key) = \(.value)"' >> emoji
curl https://raw.githubusercontent.com/shanraisshan/EmojiCodeSheet/master/json/string/Travel.json | jq '.travels.travel[] | "\(.key) = \(.value)"' >> emoji

sed 's/^"\(.*\)"$/\1/' -i emoji
