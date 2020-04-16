:-module( 'DCGblueprint',[sentence/2]).
question_starter_foodtype-->question_starter_calories.
question_starter_foodtype-->question_starter.
question_starter_ingredient-->question_starter_calories.
question_starter_ingredient-->question_starter.
question_starter_calories-->[how], [many], [calories].
question_starter_category-->question_starter,starter_category,preposition_category.
question_starter_mealtype-->question_starter,question_verb_mealtype,pronoun_subject,verb.
question_starter_verb-->[is].
question_verb1-->[does].
question_verb-->[can].
question_verb_mealtype-->[can].
calories_verb-->[left].
question_starter-->[what].
starter_category-->[kind].
preposition_category-->[of].
preposition-->[in].
preposition_mealtype-->['for'].
question_mark-->['?'].
determinant1-->[the].
determinant-->[a].
determiner-->[that].
negation-->['not'].
verb_end-->[have].
verb_end1-->[contain].
verb_past-->[ate].
verb_present-->[eat].
verb_start-->[contains].
verb1-->['do'].
verb-->[have].
pronoun_subject-->[i].
period-->['.'].
unknown-->[_].

question1and2-->question_starter_foodtype,question_verb1,unknown,verb_end1. %question1And2
question4-->question_starter,question_starter_verb,unknown.	%question4
question5-->question_starter_calories,verb1,pronoun_subject,verb,calories_verb.%question5
question6-->question_starter_category,unknown,question_verb1,unknown,verb_end1.%question6
question8-->question_starter_mealtype,preposition_mealtype,unknown,determiner,verb_start,unknown.%quesion8
question3-->question_verb_mealtype,pronoun_subject,verb,unknown,preposition_mealtype,unknown.%question3__can_i_have_Food_for_Mealtype
question7-->question_starter_verb,unknown,determinant,unknown,preposition,unknown.%question7

question-->question1and2.
question-->question3.
question-->question4.
question-->question5.
question-->question6.
question-->question7.
question-->question8.

statement1-->pronoun_subject,verb_past,unknown,preposition_mealtype,unknown. %statement1__i_ate_Food_for_Mealtype
statement2-->pronoun_subject,verb1,negation,verb_present,unknown. %statement2_i_do_not_eat_Food

statement-->statement1.
statement-->statement2.

sentence-->question.
sentence-->statement.


ingredient-->[tomato].
ingredient-->[onion].
ingredient-->[bell_pepper].
ingredient-->[chili_pepper].
ingredient-->[carrot].
ingredient-->[pea].
ingredient-->[artichoke].
ingredient-->[eggplant].
ingredient-->[cucumber].
ingredient-->[lettuce].
ingredient-->[okra].
ingredient-->[cauliflower].
ingredient-->[cabbage].
ingredient-->[broccoli].
ingredient-->[mushroom].
ingredient-->[potato].
ingredient-->[zucchini].
ingredient-->[broccoli].
ingredient-->[spinach].
ingredient-->[corn].
ingredient-->[strawberry].
ingredient-->[blackberry].
ingredient-->[blueberry].
ingredient-->[banana].
ingredient-->[orange].
ingredient-->[grape].
ingredient-->[pineapple].
ingredient-->[apple].
ingredient-->[kiwi].
ingredient-->[peaches].
ingredient-->[guava].
ingredient-->[pear].
ingredient-->[mango].
ingredient-->[apricot].
ingredient-->[avocado].
ingredient-->[cherry].
ingredient-->[fig].
ingredient-->[coconut].
ingredient-->[lemon].
ingredient-->[watermelon].
ingredient-->[cheese].
ingredient-->[milk].
ingredient-->[yogurt].
ingredient-->[flour].
ingredient-->[rice].
ingredient-->[pasta].
ingredient-->[chocolate].
ingredient-->[oil].
ingredient-->[butter].
ingredient-->[egg].
ingredient-->[fish].
ingredient-->[chicken].
ingredient-->[meat].
ingredient-->[shrimp].
ingredient-->[minced_meat].
ingredient-->[mayonnaise].
ingredient-->[vinegar].


category-->[vegetable].
category-->[fruit].
category-->[diary].
category-->[dressing].
category-->[carb].
category-->[fat].
category-->[protein].


foodtype-->[chicken_caesar_salad].
foodtype-->[green_salad].
foodtype-->[coleslaw_salad].
foodtype-->[pasta_salad].
foodtype-->[fruit_salad].
foodtype-->[croissant].
foodtype-->[spanish_omelette].
foodtype-->[boiled_egg].
foodtype-->[grilled_chicken].
foodtype-->[fried_chicken].
foodtype-->[cake].
foodtype-->[chocolate_cake].
foodtype-->[white_rice].
foodtype-->[mexican_rice].
foodtype-->[ratatouille].
foodtype-->[lasagne].
foodtype-->[pasta_white_sauce].
foodtype-->[pasta_red_sauce].
foodtype-->[pasta_alfredo].
foodtype-->[pasta_negresco].
foodtype-->[shrimp_pasta].
foodtype-->[pizza].
foodtype-->[bread].




mealtype-->[breakfast].
mealtype-->[lunch].
mealtype-->[dinner].







