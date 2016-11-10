// quicksort :: Ord a => [a] -> [a]
const quicksort = xs => {
  if(!xs.length) {
    return [];
  }
  const [y, ...ys] = xs;
  const left = quicksort(ys.filter(x => x <= y));
  const right = quicksort(ys.filter(x => x > y));
  return [...left, y, ...right];
};

const n = 4000, test = Array(n).fill().map((_, i) => n - i);
const result = quicksort(test).filter((_, i) => i < 10);
console.log(result);
