# test-gpu
The test is in main.f90. The aim of the test is to just copy to the GPU a matrix of matrices (blocks) which is in tri-diagonal form. This matrix is called H in the test. The single blocks are read from binary files. All files are in a folder called H.zip which has to be decompressed.

When trying to access the copied data on the GPU, an error occurs. In fact, by calling a subroutine (check_sum_trid) which computes for each block the sum of absolute values of each element, an error is returned on the last block.
