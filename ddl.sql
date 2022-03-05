/*
CREATE DATABASE recipe CONNECTION LIMIT 20;
COMMENT ON DATABASE recipe
    IS 'A transactional database for our favorite recipes.';

CREATE DATABASE test_recipe CONNECTION LIMIT 5;
COMMENT ON DATABASE test_recipe
    IS 'TEST instance of a transactional database for our favorite recipes.';

-- see active connections
SELECT * FROM pg_stat_activity where datname = 'recipe';

*/

-- Drop Tables if they exist
DROP TABLE IF EXISTS Food CASCADE;
DROP TABLE IF EXISTS Step CASCADE;
DROP TABLE IF EXISTS Ingredient CASCADE;
DROP TABLE IF EXISTS Measure CASCADE;
DROP TABLE IF EXISTS Food_Ingredient CASCADE;
DROP TABLE IF EXISTS FoodType CASCADE;
DROP TABLE IF EXISTS IngredientType CASCADE;
-- drop triggers and functions
DROP TRIGGER IF EXISTS food_added_trigger ON food;
DROP FUNCTION IF EXISTS food_added;

CREATE TABLE FoodType
(		
  ID serial PRIMARY KEY
  , FoodType varchar UNIQUE NOT NULL 
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE FoodType IS 'How or when we typically eat a food.';
INSERT INTO FoodType (FoodType) VALUES
    ('Breakfast')
	, ('Lunch')
	, ('Dinner')
	, ('Snack')
	, ('Dessert')
	, ('Side');

CREATE TABLE IngredientType
(		
  ID serial PRIMARY KEY
  , IngredientType varchar UNIQUE NOT NULL
  , icon varchar DEFAULT 'question'
  , color varchar
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE IngredientType IS 'Categorizes ingredients into basic food groups.';
INSERT INTO IngredientType (IngredientType,icon,color) VALUES
    ('Meat','bacon','brown')
	, ('Vegetable','carrot','orange')
	, ('Fruit','apple-alt','red')
	, ('Dairy','cheese','yellow')
	, ('Oil','fill-drip','#9B870C')
	, ('Grain','bread-slice','brown')
	, ('Vegetarian Protein','egg','gray')
	, ('Spice','mortar-pestle','gray')
	, ('Other','circle-question','yellow')
	, ('Pepper','pepper-hot','red');

CREATE TABLE Measure
(		
  ID serial PRIMARY KEY
  , Measurement varchar UNIQUE NOT NULL
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE Measure IS 'How we measure quantities of a food.';
INSERT INTO Measure (Measurement) VALUES
    ('Whole')
	, ('Pound')
	, ('Cup')
	, ('Ounce')
	, ('Clove')
	, ('Head')
	, ('Teaspoon')
	, ('Tablespoon');

CREATE TABLE Food
(		
  ID serial PRIMARY KEY
  , FoodType_ID integer NOT NULL REFERENCES FoodType(ID) ON DELETE RESTRICT 
  , Food varchar NOT NULL
  , Description varchar
  , Serving integer NOT NULL
  , Meal_Prep boolean DEFAULT 'f' NOT NULL -- might make this a foreign key to a new table: MealPrep
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE Food IS 'Usually an individual component to a meal, but can sometimes be an entire meal.';
INSERT INTO Food (FoodType_ID,Food,Description,Serving,Meal_Prep) VALUES
	(3,'Baked Chicken','Simple and versatile oven baked chicken with a variety of preparation options.',8,'f')
	, (3,'Baked Salmon','',5,'f')
	, (3,'Meatloaf','',6,'f')
	, (3,'Veggie Burgers','',5,'t')
	, (6,'Steamed Broccoli','Healthy and tasty, Stephanie''s favorite!',2,'f')
	, (3,'Orange Chicken','Easy crock-pot recipe.',8,'f')
	, (6,'Steamed Green Beans','',2,'f')
	, (3,'Egg Sandwiches','Easy if you have a toaster that can poach eggs.',1,'f')
	, (6,'Chicken Alfredo','',4,'f')
	, (2,'PB&J','A timeless classic.',1,'t')
	, (2,'Oatmeal','It''s tastier and heartier than you think.',1,'f')
	, (6,'Seared Steak','',4,'f');

CREATE TABLE Ingredient
(
  ID serial PRIMARY KEY
  , Ingredient varchar NOT NULL 
  , Measure_ID integer NOT NULL REFERENCES Measure(ID) ON DELETE RESTRICT 
  , Substitute_ID integer REFERENCES Ingredient(ID) ON DELETE SET NULL DEFAULT NULL 
  , IngredientType_ID integer NOT NULL REFERENCES IngredientType(ID) ON DELETE RESTRICT 
  , Calorie numeric(12,2)
  , Protein numeric(12,2)
  ,	Carb numeric(12,2)
  ,	Fat numeric(12,2)
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE Ingredient IS 'The components required to cook food.';
INSERT INTO Ingredient (Ingredient,Measure_ID,Substitute_ID,IngredientType_ID) VALUES
('Chicken: Boneless Skinless Breast',2,2,1)
, ('Chicken: Boneless Skinless Tenderloin',2,1,1)
, ('Bread Crumbs',3,NULL,6)
, ('Peanut Oil',4,5,5)
, ('Vegetable Oil',4,4,5)
, ('Garlic',5,NULL,8)
, ('Parmesian',4,NULL,4)
, ('Broccoli',6,NULL,2)
, ('Salt',7,NULL,8)
, ('Pepper',4,NULL,8)
, ('Quinoa',3,NULL,7)
, ('Black Beans',4,NULL,7)
, ('Water',3,NULL,9)
, ('Yellow Bell Pepper',1,NULL,2)
, ('Onion',1,NULL,2)
, ('Cumin',7,NULL,8)
, ('Sriracha',8,NULL,8)
, ('Egg',1,NULL,7)
, ('Ground Beef',2,NULL,1)
, ('Milk',4,NULL,4)
, ('Salmon Filet',2,NULL,1)
, ('Butter: Cooking',8,NULL,9);

CREATE TABLE Step
(
  ID serial PRIMARY KEY
  , Food_ID integer NOT NULL REFERENCES Food(ID) ON DELETE CASCADE 
  , ActionTime numeric(12,2)
  , Instruction_Order integer NOT NULL
  , Instruction varchar
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE Step IS 'The steps required to cook related food.';
INSERT INTO Step (Food_ID,ActionTime,Instruction_Order,Instruction) VALUES
(1,0.5,1,'Preheat Oven to 390 F.')
, (2,0.5,1,'Preheat Oven to 410 F.')
, (2,0.5,3,'Line a baking sheet with aluminum foil, enough to eventually fold over the salmon. sprinkle a tiny amount of oil where you will place the salmon.')
, (2,0.5,2,'Remove salmon from fridge, pat dry with paper towel, then sprinkle with salt, pepper, and minced garlic.')
, (2,1,4,'Melt 1 - 2 tbsp of Butter in Microwave, about 45 seconds.')
, (2,1,5,'Place salmon on baking sheet, spread melted Butter on top of Salmon.')
, (2,20,6,'Wrap salmon in foil, then place in preheated oven and cook for 12-20 minutes to an internal temperature of 125 - 135 F.')
, (1,0.5,3,'Line a baking sheet with aluminum foil.')
, (1,1,4,'Pour breadcrumbs on a paper plate and pour oil on another plate. Just enough to coat all the chicken.')
, (1,2,2,'Remove chicken from fridge and pat dry with paper towel.')
, (1,6,5,'Toss chicken in oil, then role in breadcrumbs until covered and place on baking sheet.')
, (1,15,6,'Bake chicken until internal temperature of ~160 F, around 12-15 minutes.')
, (3,3,1,'Preheat Oven to 390 F. Mix all ingredients in a large bowl except ketchup until well blended.')
, (3,1,2,'Shape meat into a loaf and place on baking sheet.')
, (3,55,3,'Bake until internal temperature of ~160 F, around 1 hour.')
, (4,20,1,'Cook Quinoa according to package instructions or follow these steps. Bring the quinoa and water to a boil in a saucepan. Reduce heat to medium-low, cover, and simmer until the quinoa is tender and the water has been absorbed, about 15 to 20 minutes.')
, (5,0.5,1,'Rinse broccolie under cold water.')
, (5,0.5,2,'Add water into bottom of steamer.')
, (5,4,3,'Chop broccoli into florets and place in top of steamer.')
, (5,6,4,'Cook broccoli with vent open for 5 - 6 minutes.')
, (5,1,5,'Place butter in steamer with lid on for about 15 seconds, then spread the softened butter over the broccoli.')
, (4,0,2,'Roughly mash the black beans with a fork leaving some whole black beans in a paste-like mixture.')
, (4,7,3,'Mix the quinoa, bread crumbs, bell pepper, onion, garlic, cumin, salt, hot pepper sauce, and egg into the black beans using your hands.')
, (4,3,4,'Form the black bean mixture into 5 patties.')
, (4,2,5,'Heat the olive oil in a large skillet.')
, (4,6,6,'Cook the patties in the hot oil until heated through, 2 to 3 minutes per side.');

CREATE TABLE Food_Ingredient
(
  Food_ID integer NOT NULL REFERENCES Food(ID) ON DELETE CASCADE 
  , Ingredient_ID integer NOT NULL REFERENCES Ingredient(ID) ON DELETE CASCADE 
  , QTY numeric(12,2) NOT NULL DEFAULT 1
  , Optional boolean DEFAULT 'f' NOT NULL
  , Created_Date timestamptz DEFAULT now() NOT NULL
  , Mod_Date timestamptz DEFAULT now() NOT NULL
);
COMMENT ON TABLE Food_Ingredient IS 'Relating food with the ingredients needed to cook it.';
INSERT INTO Food_Ingredient (Food_ID,Ingredient_ID,QTY,Optional) VALUES
(1,1,2,'f')
, (1,3,1.5,'f')
, (1,4,2,'f')
, (1,6,4,'t')
, (1,7,3,'t')
, (5,8,1,'f')
, (5,6,2,'t')
, (5,9,1,'t')
, (5,10,1,'t')
, (4,3,0.5,'f')
, (4,12,1,'f')
, (4,11,1,'f')
, (4,13,1,'f')
, (4,14,1,'t')
, (4,15,1,'f')
, (4,6,1,'f')
, (4,16,1,'f')
, (4,9,1,'f')
, (4,17,1,'f')
, (4,18,1,'f')
, (4,5,1,'f')
, (3,18,2,'f')
, (3,19,2,'f')
, (3,20,2,'t')
, (3,3,0.5,'f')
, (2,21,1.5,'f')
, (2,5,1,'f')
, (2,6,1,'f')
, (2,9,1,'f')
, (2,10,1,'f')
, (2,22,2,'f');

-- Views -----------------------------------------------------------
DROP VIEW v_food;
CREATE VIEW v_food AS
	SELECT f.*
	, CASE WHEN s.total_time IS NULL THEN 0 ELSE s.total_time END total_time
	, s.steps, ft.foodtype 
	FROM food f
	LEFT JOIN (SELECT food_id, SUM(actiontime) total_time, COUNT(*) steps FROM step GROUP BY food_id) s ON
		f.id = s.food_id
	LEFT JOIN foodtype ft ON
		ft.id = f.foodtype_id;
-- could count ingredients as well for an overall complexity score 
	-- e.g., complexity = normalized(total_time) + normalized(ingredient_count) + rarity of ingredients

DROP VIEW v_food_ing;
CREATE VIEW v_food_ing AS
	SELECT fi.*
		, i.ingredient, i.measure_id, i.substitute_id, i.ingredienttype_id
		, it.ingredienttype, it.icon, it.color
		, m.measurement
	FROM food_ingredient fi
	JOIN ingredient i ON
		fi.ingredient_id = i.id
	JOIN ingredienttype it ON
		i.ingredienttype_id = it.id
	JOIN measure m ON
		i.measure_id = m.id;
		
-- Triggers -------------------------------------------------------------------------------
-- add a dummy tuple for food_ingredient and step when a Food is added.
-- this is becasue at the application level, adding/editing ingredients requires a record.
-- the application layer will prevent null steps and food_ingredients for a given food.
	-- however, this could also be a check constraint in the DB.
DROP TRIGGER IF EXISTS food_added_trigger ON food;
DROP FUNCTION IF EXISTS food_added;
CREATE FUNCTION food_added() RETURNS trigger AS $$
        BEGIN
                INSERT INTO food_ingredient(food_id,ingredient_id,qty,optional)
        		VALUES(NEW.id,6,1,'t');
				
				INSERT INTO step(food_id,actiontime,instruction_order,instruction)
        		VALUES(NEW.id,5,1,'Click the edit button to change this placeholder text.');
				RETURN NEW;
        END;
$$ LANGUAGE 'plpgsql' SECURITY DEFINER;

CREATE TRIGGER food_added_trigger AFTER INSERT ON food
        FOR EACH ROW EXECUTE PROCEDURE food_added();
		
/* test food_added_trigger
INSERT INTO food(foodtype_id,food,description,serving,meal_prep)
            VALUES(1,'test','test trigger',1,'f');
*/