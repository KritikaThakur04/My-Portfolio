project

1) Average rating
SELECT ROUND(AVG(score), 2) AS average_rating
FROM reviews;

2)Rating distribution
SELECT score, COUNT(*) AS count
FROM reviews
GROUP BY score
ORDER BY score;

3)Top 10 most reviewed products
SELECT productId, COUNT(*) AS review_count
FROM reviews
GROUP BY productId
ORDER BY review_count DESC
LIMIT 10;

4)Reviews mentioning “good”
SELECT COUNT(*) 
FROM reviews
WHERE clean_text LIKE '%good%';

5)SELECT COUNT(DISTINCT productId) AS total_products
FROM reviews;

6)Average Rating Per Product
SELECT productId,
       ROUND(AVG(score), 2) AS avg_rating,
       COUNT(*) AS review_count
FROM reviews
GROUP BY productId
HAVING review_count >= 50
ORDER BY avg_rating DESC;

7)Length of Reviews (Text Functions)
SELECT 
  AVG(LENGTH(clean_text)) AS avg_review_length
FROM reviews;

8)Joining Your Two Sheets
SELECT r.ProductId,
       r.score,
       m.clean_text
FROM reviews r
JOIN reviews_ml m
ON r.review_year = m.review_year
LIMIT 10;

9)SELECT ProductId,UserId,review_date,
sentiment FROM reviews
WHERE score > (
    SELECT AVG(score)
    FROM reviews 
	limit 10
);
