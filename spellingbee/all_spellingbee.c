#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_WORD_LENGTH 40
#define MAX_SOLUTIONS 1000      // Maximum words in solution
#define MAX_PANGRAMS 100
// #define DICTIONARY_FILE "words_alpha.txt"
#define DICTIONARY_FILE "unix_words.txt" // only 78,000 words
#define SAVE_FILE "all_spellingbee.csv"

// 1  2  3  4  5  6  7
// 26*25*24*23*22*21*20 
int main() {
    char word[MAX_WORD_LENGTH];
    int solCount = 0;
    int pangCount = 0;
    int maxSolCount = 0;
    int maxPangCount = 0;
    int notSixCount;
    int isPangram;
    char CTR = 'a';
    char SIX[] = "bcdefg";
    int startTime = time(NULL);

    FILE* inFile;
    inFile = fopen(DICTIONARY_FILE, "r");
    if (inFile == NULL) {
        printf("Error: Could not open dictionary file\n");
        exit(1);
    }

    FILE* outFile;
    outFile = fopen(SAVE_FILE, "w");
    if (outFile == NULL) {
        printf("Error: Could not open save file\n");
        exit(1);
    }
    fprintf(outFile, "Center, Six, Solutions, Pangrams\n");
    printf("Time: %ld\n", time(NULL));
    for (int i = 0; i < 26; i++) {
        CTR = 'a' + i;
        printf("Center: %c\n", CTR);
        printf("Elapsed Time: %ld\n", time(NULL) - startTime);
        for (int j = 0; j < 26; j++) {
            SIX[0] = 'a' + j;
            if (SIX[0] == CTR) {
                continue;
            }
            for (int k = 0; k < 26; k++) {
                SIX[1] = 'a' + k;
                if (SIX[1] == CTR || SIX[1] == SIX[0]) {
                    continue;
                }
                for (int l = 0; l < 26; l++) {
                    SIX[2] = 'a' + l;
                    if (SIX[2] == CTR || SIX[2] == SIX[0] || SIX[2] == SIX[1]) {
                        continue;
                    }
                    for (int m = 0; m < 26; m++) {
                        SIX[3] = 'a' + m;
                        if (SIX[3] == CTR || SIX[3] == SIX[0] || SIX[3] == SIX[1] || SIX[3] == SIX[2]) {
                            continue;
                        }
                        for (int n = 0; n < 26; n++) {
                            SIX[4] = 'a' + n;
                            if (SIX[4] == CTR || SIX[4] == SIX[0] || SIX[4] == SIX[1] || SIX[4] == SIX[2] || SIX[4] == SIX[3]) {
                                continue;
                            }
                            for (int o = 0; o < 26; o++) {
                                SIX[5] = 'a' + o;
                                if (SIX[5] == CTR || SIX[5] == SIX[0] || SIX[5] == SIX[1] || SIX[5] == SIX[2] || SIX[5] == SIX[3] || SIX[5] == SIX[4]) {
                                    continue;
                                }
    solCount = 0;
    pangCount = 0;
    while (fgets(word, MAX_WORD_LENGTH, inFile) != NULL) {
        word[strcspn(word, "\n")] = '\0'; // Remove newline
        if (strlen(word) < 4 || strchr(word, CTR) == NULL) {
            continue;
        }
        notSixCount = 0;
        for (int i = 0; i < strlen(word); i++) {
            if (strchr(SIX, word[i]) == NULL && CTR != word[i]) {
                notSixCount++;
            }
        }
        if (notSixCount > 0) {
            continue;
        }
        solCount++;
        if (solCount >= MAX_SOLUTIONS) {
            printf("Error: Too many solutions!\n");
            exit(1);
        }
        isPangram = 0;
        for (int i = 0; i < strlen(SIX); i++){
            if (strchr(word, SIX[i]) > 0) {
                isPangram++;
            }
        }
        if (isPangram == 6) {
            pangCount++;
            // printf("Pangram found! %s\n", word);
            if (pangCount >= MAX_PANGRAMS) {
                printf("Error: Too many pangrams!\n");
                exit(1);
            }
        }
    }
    if (solCount > maxSolCount) {
        maxSolCount = solCount;
        printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
        fprintf(outFile, "%c, %s, %d, %d\n", CTR, SIX, solCount, pangCount);
    }
    if (pangCount > maxPangCount) {
        maxPangCount = pangCount;
        printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
        fprintf(outFile, "%c, %s, %d, %d\n", CTR, SIX, solCount, pangCount);

    }

    // printf("{Center: %c, Six: %s, Solultions: %d, Pangrams: %d}\n", CTR, SIX, solCount, pangCount);
    rewind(inFile);
    }
    }
    }
    }
    }
    }
    }
    printf("Total time: %ld\n", time(NULL) - startTime);
    fclose(inFile);
    fclose(outFile);
    return 0;
}

