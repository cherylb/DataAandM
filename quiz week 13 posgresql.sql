CREATE TABLE books (
bookid SERIAL PRIMARY KEY, 	
title VARCHAR UNIQUE,
qty INT);

CREATE FUNCTION newdata(booktitle TEXT, qtysold INT) RETURNS VOID AS $$
BEGIN
	UPDATE books SET qty = qtysold WHERE title = booktitle;
	IF found THEN 
		RETURN;
	END IF;
	
	BEGIN 
		INSERT INTO books(title, qty) 
		VALUES(booktitle, qtysold);
		RETURN;
	END;
END;
$$
LANGUAGE plpgsql;	


Select newdata('book1', 23);
Select newdata('book2', 34);
Select newdata('book1', 33);
