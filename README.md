# Indebted Take Home Exercise – Message Scheduling

## Provided Information 
### Question
What time of day and day of week should we send messages to our customers to encourage them to clear their debt to the best of their capacities?

### Notes
The actual accuracy of the final model is not the key focus. We are trying to assess the methodologies you have applied to address the problem statement and ultimately arrive at your recommendations. The key aspects we are looking for can be summarised as:
1. Demonstrate a clear understanding of how to analyse a data set
2. Create a clear methodology to produce the recommendations for the sample set
3. Is the process reproducible for the purpose of future recommendations?
4. A concise explanation of how to run the process

### Restrictions
1. Must be sent within the date range 4th-10th of October 2021
2. Must be sent within compliant times based on their country as per local regulations:
   -Canada: 9am to 5pm (No weekends)
   -New Zealand: 9am to 6pm (No weekends)
   -UK: 8am to 8pm (No weekends)
3. If a customer country is unavailable they are in the same country as the client
4. Process to calculate the recommendations would be run at 12am on the 4th of October

### Data
1. clients
- The information about the client - Schema:
- id: str (The client ID)
- name: str (The client name)
- product_type: str (The type of product offered by the client) - country: str (The country where the client is based)
2. customers
- The information about the customer - Schema:
- id: str (The customer ID)
- client_id: str (The ID of the associated client)
- gender: str (The gender of the customer)
- country: str (The country where the customer is based)
- dob: str (The date of birth of the customer)
- created_at: datetime (When was the customer created)
3. messages
- The previous sent messages - Schema:
- id: str (The message ID)
- customer_id: str (The ID of the associated customer)
 - sent_at: datetime (When was the message sent localized to the assumed customer timezone)
- clicked: bool (Was the message clicked)
- converted: bool (Did the message lead to a conversion) 4. sample_customer_ids
- The customer IDs to produce recommendations on - Schema:
- customer_id: str (The customer ID)
