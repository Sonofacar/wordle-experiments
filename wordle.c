#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SAME_LETTER(letter_one, letter_two) (letter_one == letter_two)
#define IS_CORRECT (guess == word)

int random_line(int max) {
	FILE * fp;
	unsigned int result;

	fp = fopen("/dev/random", "r");
	fread(&result, sizeof(result), 1, fp);
	result %= max;
	fclose(fp);

	return result;
}

char * get_word(int line_number) {
	FILE * fp;
	char * result = NULL;
	size_t length = 0;
	ssize_t word_length;

	fp = fopen("/home/carson/projects/learn-c/projects/wordle/words", "r");
	if (fp == NULL) {
		printf("Failure to open dictionary. Exiting...\n");
		exit(EXIT_FAILURE);
	}

	for (int i = 0; i <= (line_number - 1); ++i) {
		word_length = getline(&result, &length, fp);
	}

	fclose(fp);

	return result;
}

int green_response(char * word, char * guess) {
	int response = 0;

	for (int i = 0; i <= 5; ++i) {
		if (SAME_LETTER(word[i], guess[i]))
			response += 1;
		response = response << 1;
	}
	
	return response;
}

int yellow_cycle(char * word, char letter, int number) {
	if (SAME_LETTER(letter, word[number])) {
		return 0;
	}

	for (int i = 0; i <= 5; ++i) {
		if (i == number) 
			continue;
		if (word[i] == letter)
			return 1;
	}
	
	return 0;
}

int yellow_response(char * word, char * guess) {
	int response = 0;

	for (int i = 0; i <= 5; ++i) {
		yellow_cycle(word, guess[i], i);
		response = response << 1;
	}

	return response;
}

int main (int argc, char * argv[])
{
	char * word = NULL;
	unsigned int r;
	int greens = 0;
	int yellows = 0;
	char * guess = "eeeee";

	r = random_line(4597);

	word = get_word(r);

	if (word == NULL) {
		printf("Failure to get word. Exiting...\n");
		exit(EXIT_FAILURE);
	}
	
	greens = green_response(word, guess);
	yellows = yellow_response(word, guess);

	printf("%s", word);
	printf("%x\n", greens);
	printf("%x\n", yellows);

	if (word) {
		free(word);
	}
	exit(EXIT_SUCCESS);
}
