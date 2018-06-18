
/*======================================================
===============       IMPORTS         ==================
======================================================*/

import java.util.*;
import java.io.*;
import org.jnetpcap.Pcap;
import org.jnetpcap.PcapIf;
import org.jnetpcap.packet.PcapPacket;
import org.jnetpcap.packet.PcapPacketHandler;
import org.jnetpcap.PcapBpfProgram;
import org.jnetpcap.protocol.lan.Ethernet;
import org.jnetpcap.protocol.tcpip.*;
import org.jnetpcap.protocol.network.*;
import org.jnetpcap.nio.JBuffer;
import org.jnetpcap.packet.Payload;
import org.jnetpcap.protocol.network.Arp;
import org.jnetpcap.protocol.lan.IEEE802dot2;
import org.jnetpcap.protocol.lan.IEEE802dot3;



/*======================================================
===============       CHECK SUM       ==================
======================================================*/
public class Checksumv2 {
    

    /*======================================================
    ============   CALCULATE CHECK SUM    ==================
    ======================================================*/
    public static long CalculateChecksum (byte[] Data) {
        
        int length = Data.length;
        int i = 0;

        long sum = 0;
        long data;

        while (length > 1) {
            
            data = (((Data[i] << 8) & 0xFF00) | ((Data[i + 1]) & 0xFF));
            sum += data;
            
            if ((sum & 0xFFFF0000) > 0) {
                sum = sum & 0xFFFF;
                sum += 1;
            }

            i += 2;
            length -= 2;
        }

        if (length > 0) {
            sum += (Data[i] << 8 & 0xFF00);
            
            if ((sum & 0xFFFF0000) > 0) {
                sum = sum & 0xFFFF;
                sum += 1;
            }
        }

        sum = ~sum;
        sum = sum & 0xFFFF;
        
        return sum;

    }


    /*======================================================
    =========    FROM BYTE[] => MAC STRING   ===============
    ======================================================*/
    private static String MACAsString (final byte[] MAcAddress) {
        
        final StringBuilder Data = new StringBuilder();
        
        for (byte Byte: MAcAddress) {
            
            if (Data.length() != 0) Data.append (':');

            if (Byte >= 0 && Byte < 16) Data.append ('0');

            Data.append (Integer.toHexString((Byte < 0) ? Byte + 256 : Byte).toUpperCase());
        }

        return Data.toString();
    
    }


    /*======================================================
    =========   SELECT DEVICE FROM CONSOLE   ===============
    ======================================================*/
    static PcapIf SelectDevicesByConsole() {

        int DEFAULT_DEVICE = 10;

        List <PcapIf> AllDevs = new ArrayList <PcapIf>(); 
        StringBuilder ErrorData = new StringBuilder(); 
        PcapIf SelectedDevice;
        
        int Result = Pcap.findAllDevs(AllDevs, ErrorData);

        if (Result == Pcap.NOT_OK || AllDevs.isEmpty()) {
            
            System.err.printf ("Can't read list of Devices, error is %s",
                ErrorData.toString());
            
            return null;
        }

        System.out.println("============================");
        System.out.println("==  Network Devices found ==");
        System.out.println("============================");

        try {
            
            int i = 0;
            for (PcapIf Device: AllDevs) {
                
                String Description = "No Description available";
                if (Device.getDescription() != null) 
                    Description = Device.getDescription();
                Description = "";

                final byte[] MAcAddress = Device.getHardwareAddress();

                String StrMAcAddress = (MAcAddress != null)? 
                    MACAsString(MAcAddress) : "No MAC Address";

                System.out.printf("  #%d: Name [%s] ", i, Device.getName());
                System.out.printf("MAC:[%s]\n", StrMAcAddress);

                i++;

            }

            SelectedDevice = AllDevs.get(DEFAULT_DEVICE);

            String InfoSelected = (SelectedDevice.getDescription() != null)? 
                SelectedDevice.getDescription() : SelectedDevice.getName();

            System.out.printf("\n\nChoosing '%s':\n\n\n", InfoSelected);

        }
        catch (IOException e) {
            e.printStackTrace ();
        }

        SelectedDevice = AllDevs.get(DEFAULT_DEVICE);


        return SelectedDevice;

    }




    /*================================================================
    =================             MAIN             ===================
    ================================================================*/
    public static void main (String[] Args) {
    
        StringBuilder ErrorData = new StringBuilder(); 
        PcapIf Device = SelectDevicesByConsole();

        if (Device == null) return; 

        /*=====================================
        =========    START THE PCAP   =========
        =======================================
        Remember:
            - SnapshotLength 
                Refers to the amount of actual data captured
                from each packet passing through the specified network interface.
                64*1024 = 65536 bytes
            */

        int SnapshotLength  = 64 * 1024;                // Capture all packets, no trucation
        int Flags           = Pcap.MODE_PROMISCUOUS;    // capture all packets
        int Timeout         = 10 * 1000;                // 10 seconds in millis
        
        Pcap PcapInstance = Pcap.openLive(
            Device.getName(),
            SnapshotLength,
            Flags,
            Timeout,
            ErrorData
        );

        if (PcapInstance == null) {
            System.err.printf("Error while opening device: " + ErrorData.toString());
            return;
        }


        /*=====================================
        =========       FILTER        =========
        =====================================*/
        PcapBpfProgram Filter = new PcapBpfProgram();

        String Expression = "";             // "port 80";
        int Optimize      = 0;              // 1 means true, 0 means false
        int Netmask       = 0;              // Netmask value

        int Result = PcapInstance.compile(
            Filter,
            Expression,
            Optimize,
            Netmask
        );
        
        if (Result != Pcap.OK) 
            System.out.println("Filter error: " + PcapInstance.getErr());

        PcapInstance.setFilter(Filter);



        /*=====================================
        =====    CREATE PACKET HANDLER    =====
        =====================================*/
        PcapPacketHandler<String> JPacketHandler = new PcapPacketHandler<String>() {

            public void nextPacket(PcapPacket Packet, String User) {

                /*=====================================
                =====           SHOW INFO         =====
                =====================================*/

                System.out.println("=============================================");
                System.out.println("============    PACKET    ===================");
                System.out.println("=============================================\n\n");

                System.out.printf("\nReceived at %s",new Date(Packet.getCaptureHeader().timestampInMillis()));
                System.out.printf("\nCapture Length = %-4d", Packet.getCaptureHeader().caplen());
                System.out.printf("\nOriginal Sizeh = %-4d", Packet.getCaptureHeader().wirelen());
                System.out.printf("\nUSer           = %s\n\n\n", User);

                String MACAddressOrigin  = "";
                String MACAddressDestiny = "";

                /*=====================================
                ====   SHOW RAW DATA & GET MAC'S   ====
                =====================================*/
                for (int i = 0; i < Packet.size(); i++) {
                    
                    System.out.printf("%02X ", Packet.getUByte(i));

                    if (i % 16 == 15) System.out.println("");
                    
                    if (i < 6)
                        MACAddressOrigin += String.format("%02X ", Packet.getUByte(i));
                    else if (i < 12)  
                        MACAddressDestiny += String.format("%02X ", Packet.getUByte(i));

                }
                System.out.println("\n\n\n\n");

                /*=====================================
                ====       SHOW MAC'S & TYPE       ====
                =====================================*/
                int Type = Packet.getUByte(12) * 256 + Packet.getUByte(13);
                
                System.out.println("MAC Origin  = " + MACAddressOrigin);
                System.out.println("MAC Destiny = " + MACAddressDestiny);
                System.out.printf("Type        = %04X\n", Type);


                /*=====================================
                ====        IS AN IP PACKET?       ====
                =====================================*/
                if ((Type & 0xFFFF) == 0x0800) {
                    
                    System.out.println("\nIs an IPv4 Packet!");
                    
                    byte[] PacketAsByteArray = Packet.getByteArray(0, Packet.size());

                    int IPPacketSize = (PacketAsByteArray[14] & 0x0F) * 4; 
                    byte[] IPHeader = new byte[IPPacketSize];
                    System.arraycopy(PacketAsByteArray, 14, IPHeader, 0, IPPacketSize);

                    //IPHeader[10] = 0x00;
                    //IPHeader[11] = 0x00;

                    int IPacketSize = (((IPHeader[2] << 8) & 0xFF00) | ((IPHeader[3]) & 0xFF));
                    
                    System.out.printf("Complemnt to 1 of Checksum IPv4: ");
                    System.out.printf("%04X\n", CalculateChecksum(IPHeader));


                    if (Packet.size() > (13 + IPPacketSize)) {

                        /*=====================================
                        ====       IS AN TCP PACKET?       ====
                        =====================================*/
                        if (IPHeader[9] == 0x06) {

                            System.out.println("\n\nIs an TCP Packet!");
                            
                            byte[] TCPHeader = new byte[12];
                            
                            for (int i = 0; i < 8; i++) 
                                TCPHeader[i] = IPHeader[IPPacketSize - 8 + i];

                            int TCPPacketSize = IPacketSize - IPPacketSize;

                            TCPHeader[8]  = 0x00;
                            TCPHeader[9]  = 0x06;
                            TCPHeader[10] = (byte)(TCPPacketSize & 0x0000FF00);
                            TCPHeader[11] = (byte)(TCPPacketSize & 0x000000FF);

                            byte[] TCPPacket = new byte[TCPPacketSize + 12];
                            System.arraycopy(TCPHeader, 0, TCPPacket, 0, 12);
                            System.arraycopy(PacketAsByteArray, IPPacketSize + 14, TCPPacket, 12, TCPPacketSize);

                            //TCPPacket[28] = 0x00;
                            //TCPPacket[29] = 0x00;

                            System.out.printf("Complemnt to 1 of Checksum TCP: ");
                            System.out.printf("%04X\n", CalculateChecksum(TCPPacket));
                        }

                        /*=====================================
                        ====       IS AN UDP PACKET?       ====
                        =====================================*/
                        if (IPHeader[9] == 0x11) {

                            System.out.println("\n\nIs an UDP Packet!");
                            
                            byte[] UDPHeader = new byte[12];

                            for (int i = 0; i < 8; i++)
                                UDPHeader[i] = IPHeader[IPPacketSize - 8 + i];

                            int UDPPacketSize = IPacketSize - IPPacketSize;

                            UDPHeader[8]  = 0x00;
                            UDPHeader[9]  = 0x11;
                            UDPHeader[10] = (byte)(UDPPacketSize & 0x0000FF00);
                            UDPHeader[11] = (byte)(UDPPacketSize & 0x000000FF);

                            byte[] UDPPacket = new byte[UDPPacketSize + 12];
                            System.arraycopy(UDPHeader, 0, UDPPacket, 0, 12);
                            System.arraycopy(PacketAsByteArray, IPPacketSize + 14, UDPPacket, 12, UDPPacketSize);

                            //UDPPacket[18] = 0x00;
                            //UDPPacket[19] = 0x00;

                            System.out.printf("Complemnt to 1 of Checksum UDP: ");
                            System.out.printf("%04X\n", CalculateChecksum(UDPPacket));
                        }
                    }
                }
                else 
                    System.out.println("\nNot an IPv4 Packet");

                System.out.println ("\n\nRaw Data: \n" + Packet.toHexdump ());
            }

        };



        /*=====================================
        =====          DO A LOOP          =====
        =======================================
        
            Remember:
                Fourth we enter the loop and tell it to capture 5 packets. The loop
                method does a mapping of pcap.datalink() DLT value to JProtocol ID, which
                is needed by JScanner. The scanner scans the packet Datafer and decodes
                the headers. The mapping is done automatically, although a variation on
                the loop method exists that allows the programmer to sepecify exactly
                which protocol ID to use as the data link type for this pcap interface.  
        */
        PcapInstance.loop(5, JPacketHandler, "In a Loop");

        PcapInstance.close();
    }

}

