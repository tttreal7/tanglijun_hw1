#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{  Matrix c;
    c.rows=a.rows;c.cols=a.cols;
    int row=a.rows,col=a.cols,i,j;
    if(a.rows!=b.rows || a.cols!=b.cols){
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    for(i=0;i<row;i++){
        for(j=0;j<col;j++){
            c.data[i][j]=a.data[i][j]+b.data[i][j];
        }
    }
    return c;
}

Matrix sub_matrix(Matrix a, Matrix b)
{  Matrix c;
    c.rows=a.rows;c.cols=a.cols;
    int row=a.rows,col=a.cols,i,j;
    if(a.rows!=b.rows || a.cols!=b.cols){
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    for(i=0;i<row;i++){
        for(j=0;j<col;j++){
            c.data[i][j]=a.data[i][j]-b.data[i][j];
        }
    }
    return c;
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    
    if(a.cols!=b.rows)
    {
    	printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
    	return create_matrix(0, 0);
    }
    Matrix c;
    c.rows=a.rows;c.cols=b.cols;
    int i,j;
    for(i=0;i<c.rows;i++)
    {
    	for(j=0;j<c.cols;j++){
    		int k=0,m,n;
    		for(m=0;m<a.cols;m++){
    			k+=a.data[i][m]*b.data[m][j];
    		}c.data[i][j]=k;
    	}
    }return c;
}

Matrix scale_matrix(Matrix a, double k)
{
    
    int i,j;
    Matrix c=a;
    for(i=0;i<a.rows;i++){
    	for(j=0;j<a.cols;j++){
    		c.data[i][j]=a.data[i][j]*k;
    	}
    }
    return c;
}

Matrix transpose_matrix(Matrix a)
{
    
    Matrix c;
    c.cols=a.rows;c.rows=a.cols;
    int i,j;
    for(i=0;i<c.rows;i++){
    	for(j=0;j<c.cols;j++){
    		c.data[i][j]=a.data[j][i];
    	}
    }
    return c;
}

double det_matrix(Matrix a)
{
   if(a.cols!=a.rows){
    	printf("Error: The matrix must be a square matrix.\n");
    return 0;
	}
	return digui(a.data,a.cols);
}

Matrix inv_matrix(Matrix a)
{
    if(a.cols!=a.rows){
    	printf("Error: The matrix must be a square matrix.\n");
    	return create_matrix(0, 0);
    }if(det_matrix(a)==0){
    	printf("Error: The matrix is singular.\n");
    	return create_matrix(0, 0);
	}
	Matrix b=a;/*构造伴随矩阵与逆矩阵*/ 
	Matrix c;/*第j行第i列的代数余子式矩阵*/ 
	double hls=det_matrix(a);
	c.rows=a.rows-1;c.cols=a.cols-1;
	int i,j,x,y,m,n;
	for(i=0;i<a.rows;i++){
		for(j=0;j<a.cols;j++){
			 for(x=0,m=0;x<a.rows;x++){
			 	if(x!=j){
			 	for(y=0,n=0;y<a.cols;y++){
			 		if(y!=i){
			 		c.data[m][n++]=a.data[x][y];
			 		}
			 	}m++;
			 	}
			 }b.data[i][j]=det_matrix(c)*pow(-1,i+j)/hls;
		}
	}
	return b;
}

int rank_matrix(Matrix a)
{
   
   
    int r = 0,temp,m=a.rows,n=a.cols;  
	for (int i = 0; i < m; i++)
	{
		int row, col; 
		for (col = i; col < n; col++)
		{
			int flag = 0;
			for (row = i; row < m; row++)
			{
				if (a.data[row][col] !=(double)0 )
				{
					flag = 1;
					break;
				}
			}
			if (flag) break;
		}
		if (row < m && col < n)
		{
			for (int j = col; j < n; j++)
			{
				temp = a.data[i][j]; a.data[i][j] = a.data[row][j]; a.data[row][j] = temp;
			}
			double a2;//倍数
			for (int j = i + 1; j < m; j++)
			{
				a2 = -a.data[j][col] / a.data[i][col];
				for (int k = col; k < n; k++)
				{
					a.data[j][k] += a2 * a.data[i][k];
				}
			}
			r++; 
		}
		else 
		{
			break;
		}
	}
	return r;

}

double trace_matrix(Matrix a)
{
    if(a.cols!=a.rows){
    	printf("Error: The matrix must be a square matrix.\n");
    return 0;}
    int k=0,i;
    for(i=0;i<a.cols;i++){
    	k+=a.data[i][i];
    }return k;
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}
/*行列式递归函数*/ 
void copymatrix(double a[100][100],double b[100][100],int n,int i)
{
    int x,y,j;
    for(x=1;x<n;x++)
    {
        for(y=0,j=0;y<n;y++)
        {
            if(y!=i)
            {
                b[x-1][j++]=a[x][y];
            }
        }
    }
}
int digui(double a[100][100],int n)
{
    int i,res;
    res=0;
    if(n==1)
    {
        return a[0][0];
    }
    if(n==2)
    {
        return a[0][0]*a[1][1]-a[0][1]*a[1][0];
    }
    else
    {
        double b[100][100];
        for(i=0;i<n;i++)
        {
            copymatrix(a,b,n,i);//求余子式
            res+=pow(-1,i)*a[0][i]*digui(b,n-1);
        }
    }
    return res;
}
