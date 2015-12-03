# Change the folder name to get stats files from a different folder
folder=PorterStemmer_Final_ToSubmit

cd $folder

echo "------- Language-based model --------"
# What it does: find lines with numbers | cut out correct field | replace NaNs | calculate average | format and print
printf "Mean precision:\t\t\t"
grep "^Precision = " statistics_language_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean recall:\t\t\t"
grep "^Recall = " statistics_language_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean F1-score:\t\t\t"
grep "^F1-score = " statistics_language_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean average precision:\t\t"
grep "^Average Precision = " statistics_language_based.txt | cut -d" " -f4 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
echo "-------------------------------------"

printf "\n"

echo "------ Machine learning model -------"
printf "Mean precision:\t\t\t"
grep "^Precision = " statistics_term_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean recall:\t\t\t"
grep "^Recall = " statistics_term_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean F1-score:\t\t\t"
grep "^F1-score = " statistics_term_based.txt | cut -d" " -f3 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
printf "Mean average precision:\t\t"
grep "^Average Precision = " statistics_term_based.txt | cut -d" " -f4 | sed "s/NaN/0/g" | awk '{s+=$1}END{print s/NR}' RS="\n" | xargs printf "%.3f\n"
echo "-------------------------------------"

cd ..