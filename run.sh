# Auto sym link everything in the 'home-configs' directory
cd home-configs; find | while read file; do
    if [[ -f $file ]]; then
        
        if [[ -L ~/$file ]]; then
            echo "Unlinking ~/$file"
            unlink ~/$file
        fi
        if [ -e ~/$file -a ! -L ~/$file ]; then
            echo "Moving ~/$file to ~/$file.bak"
            mv ~/$file ~/$file.bak
        fi

        echo "Symlinking $PWD/$file -> ~/$file"
        ln -s $PWD/$file ~/$file1
    fi
done