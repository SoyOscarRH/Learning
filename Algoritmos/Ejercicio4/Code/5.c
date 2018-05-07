MergeSort(a, p, r)
{
	if ( p < r )
	{
		q = parteEntera((p+r)/2);
		Merge-Sort(a, p, q);
		Merge-Sort(a, q+1,r);
		Merge(a, p, q, r);
	}
}