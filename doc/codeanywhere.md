# Instructions for using codeanywhere as your IDE

The following is meant to be step-by-step how to get a repo checked out into Codeanywhere then have a split screen editing file one side and bash terminal on the other. 

1) Log in to yout github account.
2) Login to codeanywhere, if asked, use the "login using GitHub" option.
3) Create a container with a GitHub URL
![screen shot 2019-01-22 at 12 50 08 pm](https://user-images.githubusercontent.com/1433964/51555548-d49ac400-1e45-11e9-979f-f8bc5c41136a.png)
4) Enter in your git url
![screen shot 2019-01-22 at 12 50 39 pm](https://user-images.githubusercontent.com/1433964/51555599-f7c57380-1e45-11e9-812d-b31dcaaca2a4.png)
5) Create a "Blank" container Ubuntu 16.04. Hot "create" in the bottom right.
![screen shot 2019-01-22 at 12 52 19 pm](https://user-images.githubusercontent.com/1433964/51555656-16c40580-1e46-11e9-8678-d55e70284964.png)
6) In file explorer on left, right-click on the container, select "ssh terminal".
![screen shot 2019-01-22 at 12 53 33 pm](https://user-images.githubusercontent.com/1433964/51555697-30fde380-1e46-11e9-8206-f571f8487d50.png)
7) You can right-click on the tab and select "Split Veritcal"
![screen shot 2019-01-22 at 12 53 47 pm](https://user-images.githubusercontent.com/1433964/51555773-686c9000-1e46-11e9-9a88-b5dff98efe3e.png)
8) Next, run the following commands to get started
```sh
sudo apt-get update
sudo apt-get install swi-prolog
git clone https://github.com/<your-handle>/<your-repo>
cd <your-repo>
```
9) One editor and move it to one side (while keeping your terminal open).
10) Make onea executable and try running your code. You'll run into a bunch of syntax errors.
```
chmod +x onea
./onea
```
![image](https://user-images.githubusercontent.com/1433964/51555997-fa749880-1e46-11e9-8119-135a7a91a49d.png)
10) Fix these and run them again, and you'll be set
![screen shot 2019-01-22 at 12 58 41 pm](https://user-images.githubusercontent.com/1433964/51556148-5808e500-1e47-11e9-8b3a-46b409d0eea8.png)
