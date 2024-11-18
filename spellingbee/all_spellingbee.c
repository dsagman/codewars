#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_WORD_LENGTH 40
#define MAX_WORDS 100000 // Maximum words in solution
#define MAX_PANGRAMS 100
// #define DICTIONARY_FILE "words_alpha.txt"
#define DICTIONARY_FILE "unix_words_filtered.txt" // only 78,000 words
#define SAVE_FILE "all_spellingbee.csv"
#define VOWELS "aeiou"

// 1  2  3  4  5  6  7
// 26*25*24*23*22*21*20
int main()
{
    char word[MAX_WORD_LENGTH];
    // char (*words)[MAX_WORD_LENGTH] = malloc(MAX_WORDS * MAX_WORD_LENGTH); // ok for C but not C++
    char(*words)[MAX_WORD_LENGTH] = (char(*)[MAX_WORD_LENGTH])malloc(MAX_WORDS * sizeof(*words)); // ok for C and C++
    if (words == NULL)
    {
        printf("Error: Memory allocation failed\n");
        exit(1);
    }
    int solCount = 0;
    int pangCount = 0;
    int maxSolCount = 0;
    int maxPangCount = 0;
    int wordCount = 0;
    int notSixCount;
    int isPangram;
    char CTR = 'a';
    char SIX[] = "bcdefg";
    long int startTime = time(NULL);
    long int combos = 0;
    

    FILE *inFile;
    inFile = fopen(DICTIONARY_FILE, "r");
    if (inFile == NULL)
    {
        printf("Error: Could not open dictionary file\n");
        exit(1);
    }
    while (fgets(word, MAX_WORD_LENGTH, inFile) != NULL)
    {
        word[strcspn(word, "\n")] = '\0'; // Remove newline
        strcpy(words[wordCount], word);
        wordCount++;
        if (wordCount >= MAX_WORDS)
        {
            printf("Error: Too many words in dictionary\n");
            break;
        }
    }
    fclose(inFile);

    FILE *outFile;
    outFile = fopen(SAVE_FILE, "w");
    if (outFile == NULL)
    {
        printf("Error: Could not open save file\n");
        exit(1);
    }
    fprintf(outFile, "Center, Six, Solutions, Pangrams\n");
    printf("Time: %ld\n", time(NULL));
    for (int i = 0; i < 26; i++)
    {
        CTR = 'a' + i;
        printf("Center: %c\n", CTR);
        printf("Time: %ld\n", time(NULL));
        printf("Elapsed Time: %ld\n", time(NULL) - startTime);
        printf("Combos: %ld\n", combos);
        for (int j = 0; j < 26; j++)
        {
            SIX[0] = 'a' + j;
            if (SIX[0] == CTR)
            {
                continue;
            }
            for (int k = j + 1; k < 26; k++)
            {
                SIX[1] = 'a' + k;
                if (SIX[1] == CTR)
                {
                    continue;
                }
                for (int l = k + 1; l < 26; l++)
                {
                    SIX[2] = 'a' + l;
                    if (SIX[2] == CTR)
                    {
                        continue;
                    }
                    for (int m = l + 1; m < 26; m++)
                    {
                        SIX[3] = 'a' + m;
                        if (SIX[3] == CTR)
                        {
                            continue;
                        }
                        for (int n = m + 1; n < 26; n++)
                        {
                            SIX[4] = 'a' + n;
                            if (SIX[4] == CTR)
                            {
                                continue;
                            }
                            for (int o = n + 1; o < 26; o++)
                            {
                                SIX[5] = 'a' + o;
                                if (SIX[5] == CTR)
                                {
                                    continue;
                                }
                                // at least one letter has to be a vowel
                                if (strchr(VOWELS, CTR) == NULL && strchr(VOWELS, SIX[0]) == NULL && strchr(VOWELS, SIX[1]) == NULL && strchr(VOWELS, SIX[2]) == NULL && strchr(VOWELS, SIX[3]) == NULL && strchr(VOWELS, SIX[4]) == NULL && strchr(VOWELS, SIX[5]) == NULL)
                                {
                                    printf("Center: %c, Six: %s, No Vowels\n", CTR, SIX);
                                    continue;
                                }
                                combos++;
                                solCount = 0;
                                pangCount = 0;
                                for (int w = 0; w < wordCount; w++)
                                {
                                    if (strlen(words[w]) < 4 || strchr(words[w], CTR) == NULL)
                                    {
                                        continue;
                                    }
                                    notSixCount = 0;
                                    for (int i = 0; i < strlen(word); i++)
                                    {
                                        if (strchr(SIX, words[w][i]) == NULL && CTR != words[w][i])
                                        {
                                            notSixCount++;
                                        }
                                    }
                                    if (notSixCount > 0)
                                    {
                                        continue;
                                    }
                                    solCount++;
                                    isPangram = 0;
                                    for (int p = 0; p < strlen(SIX); p++)
                                    {
                                        // if (strchr(words[w], SIX[p]) > 0) { // valid C code but not C++ code
                                        if (strchr(words[w], SIX[p]) != NULL) // this works on Windows and not yet tested on Mac and Linux
                                        { 
                                            isPangram++;
                                        }
                                    }
                                    if (isPangram == 6)
                                    {
                                        pangCount++;
                                    }
                                }
                                if (solCount > maxSolCount)
                                {
                                    maxSolCount = solCount;
                                    printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
                                    fprintf(outFile, "%c, %s, %d, %d\n", CTR, SIX, solCount, pangCount);
                                }
                                if (pangCount > maxPangCount)
                                {
                                    maxPangCount = pangCount;
                                    printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
                                    fprintf(outFile, "%c, %s, %d, %d\n", CTR, SIX, solCount, pangCount);
                                }

                                // printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
                            }
                        }
                    }
                }
            }
        }
    }
    printf("Total time: %ld\n", time(NULL) - startTime);
    printf("Total Combos: %ld\n", combos);
    fclose(outFile);
    free(words);
    return 0;
}
