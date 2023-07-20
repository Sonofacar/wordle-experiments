#include <stdio.h>
#include <stdlib.h>

#define DICTIONARY_PATH				("/home/carson/projects/woRdle/words")
/*#define DICTIONARY_PATH				("/auto/fsj/carson/c-learning/projects/wordle-repo/words")*/
#define SAME_LETTER(letter_one, letter_two)	(letter_one == letter_two)
#define IS_CORRECT				(guess == word)
#define BINARY_PATTERN				("%c%c%c%c%c\n")
#define BINARY_EXTRACTION(num) \
	((num) & 0x10 ? '1' : '0'), \
	((num) & 0x08 ? '1' : '0'), \
	((num) & 0x04 ? '1' : '0'), \
	((num) & 0x02 ? '1' : '0'), \
	((num) & 0x01 ? '1' : '0') 

int index_to_int (int index)
{
	int reference[5] = {0x10, 0x08, 0x04, 0x02, 0x01};
	
	if (index == -1)
		return 0;

	return reference[index];
}

int random_line(int max)
{
	FILE * fp;
	unsigned int result;

	fp = fopen("/dev/random", "r");
	fread(&result, sizeof(result), 1, fp);
	result %= max;
	fclose(fp);

	return result;
}

char * get_word(int line_number)
{
	FILE * fp;
	char * result = NULL;
	size_t length = 0;
	ssize_t word_length;

	fp = fopen(DICTIONARY_PATH, "r");
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

int green_response(char * word, char * guess)
{
	int response = 0;

	for (int i = 0; i <= 4; i++) {
		if (SAME_LETTER(word[i], guess[i]))
			response += 1;
		if (i == 4){
			return response;
		}
		response = response << 1;
	}
}

int yellow_cycle(char * word, char letter, int number, int indices[5])
{
	if (SAME_LETTER(letter, word[number])) {
		indices[number] = -1;
		return 0;
	}

	for (int i = 0; i <= 4; i++) {
		if (i == number) 
			continue;
		if (word[i] == letter) {
			indices[number] = i;
			return 1;
		}
	}
	
	return 0;
}

int yellow_response(char * word, char * guess, int indices[5])
{
	int response = 0;

	for (int i = 0; i <= 4; i++) {
		response += yellow_cycle(word, guess[i], i, indices);

		if (i == 4) {
			return response;
		}

		response = response << 1;
	}
}

int forward_check(char * word, char letter, int number, int indices[5], int greens) 
{
	int response = 0;

	for (int i = number + 1; i <= 4; i++) {
		if (index_to_int(number) & greens)
			break;

		if (letter == word[i]) {
			response += index_to_int(i);
			indices[number] = i;
		}
	}
	return response;
}

int yellows_fix(char * word, char * guess, int indices[5], int greens, int yellows)
{
	int difference = 0;
	int correction = 0;

	for (int i = 0; i <= 4; i++) {
		if (indices[i] == -1)
			continue;

		for (int j = i - 1; j >= 0; --j) {
			if (indices[j] == indices[i]) {
				indices[i] = -1;
				difference += index_to_int(i);
				break;
			}
			
		}

		if (index_to_int(indices[i]) & greens) {
			indices[i] = -1;
			difference += (index_to_int(i) - forward_check(word, guess[i], i, indices, greens));
		}

		/*
		if ((indices[i] == -1) && ((yellows & index_to_int(i)) != 0))
			yellows -= index_to_int(i);
		*/
	}

	yellows -= difference;

	printf("{ %d, ", indices[0]);
	printf("%d, ", indices[1]);
	printf("%d, ", indices[2]);
	printf("%d, ", indices[3]);
	printf("%d }\n", indices[4]);

	return yellows;
}

int main (int argc, char * argv[])
{
	char * word = NULL;
	unsigned int r;
	int greens = 0;
	int yellows = 0;
	char * guess = "peeks";
	int yellow_indices[5] = {-1, -1, -1, -1, -1};

	r = random_line(4597);

	/*word = get_word(r);*/
	word = "kneed\n";

	if (word == NULL) {
		printf("Failure to get word. Exiting...\n");
		exit(EXIT_FAILURE);
	}
	
	greens = green_response(word, guess);
	yellows = yellow_response(word, guess, yellow_indices);
	yellows = yellows_fix(word, guess, yellow_indices, greens, yellows);

	printf("%s", word);
	printf("%s\n", guess);
	printf(BINARY_PATTERN, BINARY_EXTRACTION(greens));
	printf(BINARY_PATTERN, BINARY_EXTRACTION(yellows));

	if (word) {
		free(word);
	}
	exit(EXIT_SUCCESS);
}
