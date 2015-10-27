﻿#include <WinSock2.h>
#include <stdio.h>
#include <stdlib.h>

#include<string>
#include<iostream>
#pragma comment(lib, "ws2_32.lib")

using namespace std;

string GetLocalIpAddress()
{
	WORD wVersionRequested = MAKEWORD(2, 2);

	WSADATA wsaData;
	if (WSAStartup(wVersionRequested, &wsaData) != 0)
		return "";

	char local[255] = { 0 };
	gethostname(local, sizeof(local));
	hostent* ph = gethostbyname(local);
	if (ph == NULL)
		return "";

	in_addr addr;
	memcpy(&addr, ph->h_addr_list[0], sizeof(in_addr)); // 这里仅获取第一个ip  

	string localIP;
	localIP.assign(inet_ntoa(addr));

	WSACleanup();
	return localIP;
}

void main()
{
	WSADATA wsaData;

	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0)
	{
		printf("Failed to load Winsock");
		return;
	}
	int port = 5099;

	//创建用于监听的套接字
	SOCKET sockSrv = socket(AF_INET, SOCK_STREAM, 0);
	SOCKADDR_IN addrSrv;
	addrSrv.sin_family = AF_INET;
	addrSrv.sin_port = htons(port); //1024以上的端口号
	addrSrv.sin_addr.S_un.S_addr = inet_addr(GetLocalIpAddress().c_str());

	printf("Server IP:[%s]\n", inet_ntoa(addrSrv.sin_addr));

	int retVal = bind(sockSrv, (LPSOCKADDR)&addrSrv, sizeof(SOCKADDR_IN));
	if (retVal == SOCKET_ERROR){
		printf("Failed bind:%d\n", WSAGetLastError());
		return;
	}

	if (listen(sockSrv, 10) == SOCKET_ERROR){
		printf("Listen failed:%d", WSAGetLastError());
		return;
	}

	SOCKADDR_IN addrClient;
	int len = sizeof(SOCKADDR);

	char buf[] = "Server: hello, I am a server.....";

	while (1)
	{
		//等待客户请求到来	
		SOCKET sockConn = accept(sockSrv, (SOCKADDR *)&addrClient, &len);
		if (sockConn == SOCKET_ERROR){
			printf("Accept failed:%d", WSAGetLastError());
			break;
		}

		printf("Accept client IP:[%s]\n", inet_ntoa(addrClient.sin_addr));

		//发送数据
		int iSend = send(sockConn, buf, sizeof(buf), 0);
		if (iSend == SOCKET_ERROR){
			printf("send failed");
			break;
		}

		char recvBuf[100];
		memset(recvBuf, 0, sizeof(recvBuf));
		// 		//接收数据
		recv(sockConn, recvBuf, sizeof(recvBuf), 0);
		printf("%s\n", recvBuf);

		closesocket(sockConn);
	}

	closesocket(sockSrv);
	WSACleanup();
	system("pause");
}