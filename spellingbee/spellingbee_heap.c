#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_WORD_LENGTH 40
#define MAX_WORDS 372000      // Maximum words in solution
#define DICTIONARY_FILE "words_alpha.txt"
#define CTR 'i'
#define SIX "todrym"

int main() {
    char word[MAX_WORD_LENGTH];
    char (*words)[MAX_WORD_LENGTH] = malloc(MAX_WORDS * MAX_WORD_LENGTH);
    if (words == NULL) {
        printf("Error: Memory allocation failed\n");
        exit(1);
    }
    int wordCount = 0;
    int solCount = 0;
    int pangCount = 0;
    int notSixCount;
    int isPangram;

    FILE* file;
    file = fopen(DICTIONARY_FILE, "r");
    if (file == NULL) {
        printf("Error: Could not open dictionary file\n");
        exit(1);
    }
    while (fgets(word, MAX_WORD_LENGTH, file) != NULL) {
        word[strcspn(word, "\n")] = '\0'; // Remove newline
        strcpy(words[wordCount], word);
        wordCount++;
        if (wordCount >= MAX_WORDS) {
            printf("Error: Too many words in dictionary\n");
            break;
        }
    }
    fclose(file);

    solCount = 0;
    pangCount = 0;
    for (int w = 0; w < wordCount; w++) {
        if (strlen(words[w]) < 4 || strchr(words[w], CTR) == NULL) {
            continue;
        }
        notSixCount = 0;
        for (int i = 0; i < strlen(words[w]); i++) {
            if (strchr(SIX, words[w][i]) == NULL && CTR != words[w][i]) {
                notSixCount++;
            }
        }
        if (notSixCount > 0) {
            continue;
        }
        solCount++;
        isPangram = 0;
        for (int i = 0; i < strlen(SIX); i++){
            if (strchr(words[w], SIX[i]) > 0) {
                isPangram++;
            }
        }
        if (isPangram == 6) {
            pangCount++;
        }
    }

    printf("Center: %c, Six: %s, Solutions: %d, Pangrams: %d\n", CTR, SIX, solCount, pangCount);

    return 0;
}

