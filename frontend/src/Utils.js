exports._setLocalStorage = (k) => (v) => () => {
  if (typeof window !== "undefined") {
    localStorage.setItem(k, v);
  }
};
