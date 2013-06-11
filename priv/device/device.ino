#include <Arduino.h>
#include <SoftwareSerial.h>
#include "WiFly.h"

#define DEBUG // comment me out to disable Debug info
#ifdef DEBUG
#define DBG0(message)    Serial.print(message)
#define DBG(message)    Serial.println(message)
#else
#define DBG0(message)
#define DBG(message)
#endif // DEBUG

#define HOST      "192.168.1.11"
#define PORT      10000
#define CONNECT_TIMEOUT 1000

#define SSID      "wolf"
#define KEY       "JohnsonLau"
#define AUTH      WIFLY_AUTH_WPA2_PSK

#define RECV_BUFFER_LEN      100

byte mac[20];
unsigned char macLen = 0;

unsigned int lastVoltage = 0;
unsigned int voltageCachedCount = 0;

unsigned int i, j;

char recv_buffer[RECV_BUFFER_LEN];
unsigned int byteCount;

WiFly wifly(2, 3); //wifly(TX, RX)

void setup() {
#ifdef DEBUG
  Serial.begin(9600);
#endif
  
  DBG("--------- MY WIFLY DEVICE --------");
  
  // wait for initilization of wifly
  delay(3000);
  
  initDev();
}

void loop() {    
  byteCount = wifly.receive((uint8_t *)recv_buffer, RECV_BUFFER_LEN, 50);  
  if(byteCount > 0)
  {
    recv_buffer[byteCount] = '\0';
    String receivedStr = recv_buffer;    
    DBG(receivedStr);
    
    // re-connect TCP
    if(receivedStr.indexOf("*CLOS*") != -1)
    {
      delay(5000);
      connectServer();
    }
  }
  
  monitorVoltage();
  
  delay(50);
}

void monitorVoltage()
{ 
  unsigned int sensorVal = analogRead(A0);
  voltageCachedCount ++;
  if(abs(sensorVal - lastVoltage) > 2 || voltageCachedCount == 50)
  {
    voltageCachedCount = 0;
    lastVoltage = sensorVal;
    DBG(sensorVal);
    
    byte byte1 = sensorVal/256;
    byte byte2 = sensorVal%256;
    
    wifly.write('B');
    wifly.write(byte1);
    wifly.write(byte2);  
  }
}

void initDev()
{ 
  wifly.reset();
  wifly.sendCommand("set wlan join 0\r");  
  
  macLen = getMac(mac); 
  printMac();
  
  joinAP();
  connectServer();  
  
  // back to data mode
  while(!wifly.dataMode())
  {
      delay(1000);
  }
  
  DBG("init done.");
}

void joinAP()
{ 
  DBG("joining " SSID);
  while(!wifly.join(SSID, KEY, AUTH))
  {
    DBG("  retrying...");
    delay(3000);
  }
  DBG("  joined " SSID );
}

void connectServer()
{  
  DBG("connecting " HOST);
  while(!wifly.connect(HOST, PORT, CONNECT_TIMEOUT))
  {
    DBG("  retrying...");
    delay(3000);
  }
  DBG("  connected " HOST );
  
  sendOnlineData();
}

void sendOnlineData()
{
  wifly.write('A');
  
  for(i=0; i < macLen; i++)
  {
    if(mac[i] != 0x3A)
    {
      wifly.write(mac[i]);
    }
  }
}

int getMac(byte *mac)
{ 
  int macLen = 0;
  
  wifly.clear();
  
  wifly.sendCommand("get mac\r");
  byteCount = wifly.receive((uint8_t *)recv_buffer, RECV_BUFFER_LEN, 5000);
  if(byteCount > 0)
  {
    recv_buffer[byteCount] = '\0';
    String receivedStr = recv_buffer;
    
    receivedStr = receivedStr.substring(receivedStr.indexOf('=') + 1, receivedStr.indexOf('=') + 18);
    receivedStr.toUpperCase();
    int len = receivedStr.length() + 1;    
    byte temp[len];
    receivedStr.getBytes(temp, len);
    
    // cut the colons (":") in the mac
    j = 0;
    for(i = 0;i < len; i++)
    {
      if(temp[i] != 0x3A)
      {
        mac[j] = temp[i];
        j++;
      }
    }
    macLen = j - 1;
  }  
  
  return macLen;
}

void printMac()
{
  DBG0("mac: ");
  for(i = 0;i < macLen; i++)
  {
    DBG0((char)mac[i]);
  }
  DBG("");
}
