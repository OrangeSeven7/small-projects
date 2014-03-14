#A simple algorithm for radix (O(n))sorting and grouping Shares by the User who created them.
#Works equally well on sparsely populated data structures, in terms of both space and complexity.
def radix_sort(ids)
  share_hash = Hash[ids.map {|x| [x,[]]}]
  Share.where(user_id: ids).each do |t|
    share_hash[t.user_id] = share_hash[t.user_id] + [t]
  end
end

#A recursive algorithm for integer sorting. Best time O(n) worst time O(nlog(n)),
#where log is base 256 (so it maxes out at essentially O(n) for 32 bit ints, where c = 4*previous c)
#Memory usage at any given time maxes out at the 1024 times integer size
def reorder(ints)
  array = Array.new(256,[])
  level = 2^24
  ints.each do |t|
    array[(t/level)%256] = t
  end
  array.each do |t|
    if t.length > 1
      reorder_helper(t,level/(2^8))
      t = []
    end
  end
  array.flatten
end

def reorder_helper(ints, level)
  array = Array.new(256,[])
  ints.each do |t|
    array[(t/level)%256] = t
  end
  array.each do |t|
    if t.length > 1
      reorder_helper(t,level/(2^8))
    end
  end
end

