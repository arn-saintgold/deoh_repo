# emo_triplets_combo_in_comments_by_leaning.tsv

each column contains p(_e2_ & e_3 | _e1_ & _l_), where _e1_, _e2_, and _e3_ are three Plutchik emotions, and _l_ is an user's leaning (Reliable or Questionable)
We set to 0 non significant values, i.e. values less than mean(P) + 2*sd(p), where P is the distribution of p(_e2_ & e_3 | _e1_ & _l_) from the bootstrapped values
If _e1_ is NA, then the row shows p(_e2_ & e_3 | _l_), i.e. the probability of finding couples of emotions given the leaning.
Moreover, the row shows p(_e2_ | _e1_ & _l_) if _e2_ = _e3_, i.e. the probability of finding an emotion conditioned on a second emotion and the leaning

The tsv file contains 11 columns:
- emo1
- emo2
- is_questionable
- eight 'has_X' colums, where X is an emotion
- is_not_bootstrapped

## emo1
The emotion on which we condition, i.e. _e1_.; The counts include only comments with significant _e1_ signal.

## emo2
The first emotion we want to find probabilities of.

## is_questionable
An integer in {0,1} representing the leaning _l_.
If it is 0, the comments come from Mainstream-prome users; viceversa they come from Misinformation-prone users.

## has_X
The X is _e3_, and the column contains the probability shown above.

## is_not_bootstrapped

Vestigial column. Included for compatibility in previous code.
It used to indicate if the statistic came from a permutation of the emotions in the comment.