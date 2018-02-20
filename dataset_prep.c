#include <stdio.h>

int main(){
	FILE *fp,*nfp;
	fp = fopen("d.txt","r");
	nfp = fopen("new.txt","w");
	fprintf(nfp,"((");
	char c;
	while(!feof(fp)){
		c = fgetc(fp);
		while(c!=' '){
			c= fgetc(fp);
		}
		c = fgetc(fp);
		c= fgetc(fp);
		while(c != '\n' && !feof(fp)){
			if(c!='\t' && c!=',')
			fputc(c,nfp);
			c= fgetc(fp);
		}
		fputc(')',nfp);
		if(!feof(fp)){
			fputc(',',nfp);
			fputc('\n',nfp);
			fputc('(',nfp);
		}
		else{
			fputc(')',nfp);
		}
	}
	return 0;
}