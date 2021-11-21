getSplits = function(t, df) {
  ni <- nodeids(t)
  ni_terminal <- nodeids(t, terminal = TRUE)
  ni_inner <- ni[!ni %in% ni_terminal]
  tree.splits = sapply(ni_inner, function(i) split_node(node_party(t[[i]]))$breaks)
  tree.splits = tree.splits[order(tree.splits)]
  interval.boundaries = c(min(df$x), tree.splits, max(df$x))
  
  for (i in 1:(length(interval.boundaries) - 1)) {
    if (interval.boundaries[i + 1] != max(df$x)) {
      interval.subset = df[df$x >= interval.boundaries[i] & df$x < interval.boundaries[i + 1], ]
    } else {
      interval.subset = df[df$x >= interval.boundaries[i] & df$x <= interval.boundaries[i + 1], ]
    }
    
    df[rownames(interval.subset), 'subspace'] = rep(i, nrow(interval.subset))
    df[rownames(interval.subset), 'cAME'] = rep(mean(interval.subset$me), nrow(interval.subset))
    df[rownames(interval.subset), 'cANLM'] = rep(mean(interval.subset$nlm), nrow(interval.subset))
  }
  return(df)
}
