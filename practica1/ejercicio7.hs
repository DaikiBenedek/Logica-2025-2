-- Función de paréntesis balanceados:
esBalanceada :: String -> Int -> Bool
esBalanceada [] 0 = True
esBalanceada [] _ = False
esBalanceada (x:xs) n
  | n < 0     = False
  | x == '('  = esBalanceada xs (n + 1)
  | x == ')'  = esBalanceada xs (n - 1)
  | otherwise = esBalanceada xs n
