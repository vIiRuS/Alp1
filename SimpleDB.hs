module SimpleDB where


data Datenbank	a b = Datenbank [(a,[b])]
	deriving Show

create :: Datenbank a b
create = Datenbank []

select :: Eq a => Datenbank a b -> a -> [b]
select (Datenbank db) key = getValue db key
	where
		getValue [] _ = []
		getValue ((a,b):xs) key
			| a == key = b
			| otherwise = getValue xs key

update :: Eq a => Eq b => Datenbank a b -> a -> [b] -> Datenbank a b
update (Datenbank db) key value = Datenbank (updateValue db key value)
	where
		--updateValue :: [(a,b)] -> a -> b -> [(a,b)]
		updateValue [] key value = [(key, value)]
		updateValue ((a,b):xs) key value
			| a == key && b == value = (a,b):xs
			| a == key = (a,value):xs
			| otherwise = (a,b):updateValue xs key value


--drop :: Eq a => Eq b => Datenbank a b -> a -> [b] -> Datenbank a b
drop (Datenbank db) key value = Datenbank (dropValue db key value)
	where
		dropValue [] _ _ = error "Dieser Schlüssel existiert nicht"
		dropValue ((a,b):xs) key value
			| a == key && (filter (/=value) b) == []= xs
			| a == key = (a, (filter (/=value) b)):xs
			| otherwise = (a,b):(dropValue xs key value)

testDB = Datenbank [("1", [1,2,3]), ("2", [2,3,4]), ("3", [3,4,5]), ("4", [4,5,6])]