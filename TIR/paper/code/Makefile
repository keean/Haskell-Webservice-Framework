all: index.html HList.zip

index.html: pre.html README post.html
	cat pre.html README post.html > index.html

HList.zip:
	mkdir -p HList
	cp --preserve *.hs README HList
	zip -r HList.zip HList
	rm -rf HList

clean:
	rm -f index.html HList.zip
	rm -f *~
