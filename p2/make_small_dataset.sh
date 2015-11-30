mkdir data_small
cp data/allZips/ap*.zip data_small/allZips
cp data/topics data_small
cat data/qrels | grep AP > data_small/qrels