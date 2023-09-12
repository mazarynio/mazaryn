import openai
import sys

# Set your OpenAI API key
api_key = "YOUR_API_KEY"

# Initialize the OpenAI API client
openai.api_key = api_key

def chat_with_gpt3(prompt):
    # Define a conversation with the chatbot
    conversation = [
        {"role": "system", "content": "You are a helpful assistant."},
        {"role": "user", "content": prompt}
    ]

    # Generate a response from the chatbot
    response = openai.ChatCompletion.create(
        model="gpt-3.5-turbo",
        messages=conversation
    )

    return response['choices'][0]['message']['content']

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python chat_with_gpt3.py <user_input>")
        sys.exit(1)

    user_input = sys.argv[1]
    bot_response = chat_with_gpt3(user_input)
    print("ChatGPT: " + bot_response)
