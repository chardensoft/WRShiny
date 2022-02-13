#!/bin/bash

cp www/runners.json ../json_to_firestore/extra
rm -rf ../json_to_firestore/files
mkdir ../json_to_firestore/files
mv ../json_to_firestore/extra/runners.json ../json_to_firestore/files/
sed -i '' 's/.id/rid/g' ../json_to_firestore/uploader.js
node ../json_to_firestore/uploader.js

cp www/Wrunners.json ../json_to_firestore/extra
rm -rf ../json_to_firestore/files
mkdir ../json_to_firestore/files
mv ../json_to_firestore/extra/Wrunners.json ../json_to_firestore/files/
sed -i '' 's/.id/rid/g' ../json_to_firestore/uploader.js
node ../json_to_firestore/uploader.js

cp www/teams.json ../json_to_firestore/extra
rm -rf ../json_to_firestore/files
mkdir ../json_to_firestore/files
mv ../json_to_firestore/extra/teams.json ../json_to_firestore/files/
sed -i '' 's/.id/tid/g' ../json_to_firestore/uploader.js
node ../json_to_firestore/uploader.js

cp www/Wteams.json ../json_to_firestore/extra
rm -rf ../json_to_firestore/files
mkdir ../json_to_firestore/files
mv ../json_to_firestore/extra/Wteams.json ../json_to_firestore/files/
sed -i '' 's/.id/tid/g' ../json_to_firestore/uploader.js
node ../json_to_firestore/uploader.js

