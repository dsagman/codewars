#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_WORD_LENGTH 40
#define MAX_SOLUTIONS 1000      // Maximum words in solution
#define MAX_PANGRAMS 100
#define DICTIONARY_FILE "words_alpha.txt"
#define CTR 'i'
#define SIX "todrym"

int main() {
    char word[MAX_WORD_LENGTH];
    char solution[MAX_WORDS][MAX_WORD_LENGTH];
    char pangrams[MAX_PANGRAMS][MAX_WORD_LENGTH];
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
        strcpy(solution[solCount], word);
        solCount++;
        if (solCount >= MAX_WORDS) {
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
            strcpy(pangrams[pangCount], word);
            pangCount++;
            if (pangCount >= MAX_PANGRAMS) {
                printf("Error: Too many pangrams!\n");
                exit(1);
            }
        }
    }

    for (int i = 0; i < solCount; i++) {
        printf("%s\n", solution[i]);
    }
    for (int i = 0; i < pangCount; i++) {
        printf("Pangram: %s\n", pangrams[i]);
    }
    printf("Found %d solutions\n", solCount);

    // free memory
    

    fclose(file);
    return 0;
}

