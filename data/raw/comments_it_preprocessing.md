# Italian YouTube COVID-19 Comments

Data employed in 'dynamics of online hate' paper.

## Columns:

There are 9 columns and 1.273.929 rows.
Columns are: ID_Commento, ID_Video, Published_At, Nome_Utente, Testo, parent_id, csv_id, Label, is_questionable.

#### ID_Commento

YouTube Comment ID. There are 1.273.929 Comments.

#### ID_Video
Video ID. There are 26.266 video IDs

#### Published_At
Time the comment was posted. Ranges from 2019-12-01 to 2020-06-09.

#### Nome_Utente
Usernames. There are 324.005 distinct user names.

#### Testo
Comments' text. The shortest and the longest are, respectively, 2 and 9.840 characters long.

#### parent_id
YouTube ID of the parent comment

#### csv_id
Anonymized comment ID

#### Label
Toxicity label from 'dynamics of online hate' paper.
Can take one of four values:
- 0. appropriato
- 1. inappropriato
- 2. offensivo
- 3. violento

#### is_questionable
Boolean indicator variable. True if the comment was posted on a misinformation channel, False otherwise.


